{-# LANGUAGE OverloadedStrings #-}
module BlockSite (checkedRoutes, blockRules, sitemapItems, generatedMtime) where

import BlockModel
import BlockViews
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import Data.List (stripPrefix)
import qualified Data.Text as T
import qualified Data.Text.Normalize as Unicode
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hakyll
import Hakyll.Core.Rules.Internal (Rules(..), RuleSet(..), rulesProvider)
import Network.URI (unEscapeString)
import System.Directory (doesFileExist, removeFile, renameFile)
import System.IO.Temp (withTempDirectory)
import System.FilePath
import Text.Read (readMaybe)
import qualified Text.Pandoc as P
import Text.Pandoc.Walk (query, walk)

-- Inspect the routes Hakyll will actually run (including versions such as tag RSS).
-- Hakyll itself keeps the first compiler for a given Identifier.
type PathKey = FilePath -> FilePath

checkedRoutes :: Configuration -> (PathKey -> Rules ()) -> Rules ()
checkedRoutes conf rules = Rules $ do
  key <- liftIO $ pathComparison (storeDirectory conf)
  (_, registered) <- RWS.listen (unRules (rules key))
  provider <- RWS.asks rulesProvider
  liftIO $ do
    outputs <- forM (S.toList $ S.fromList $ map fst $ rulesCompilers registered) $ \ident -> do
      (path, _) <- runRoutes (rulesRoutes registered) provider ident
      pure $ fmap (\p -> (key p, show ident)) path
    let routes = catMaybes outputs
        files = M.fromList routes
    unique "Hakyll" "output path" routes
    -- A file cannot also be a directory containing another output.
    forM_ routes $ \(path,origin) ->
      forM_ (parents path) $ \parent -> forM_ (M.lookup parent files) $ \other ->
        failAt origin ("output path " ++ path ++ " lies under " ++ parent ++ " from " ++ other)
  where
    parents path = case takeDirectory path of
      dir | dir == "." || dir == path -> []
          | otherwise -> dir : parents dir

-- The checkout may use a case-sensitive or case-insensitive filesystem. Check its
-- spelling rules once, before compiling, including canonical Unicode equivalents.
pathComparison :: FilePath -> IO PathKey
pathComparison cache = withTempDirectory cache "route-probe" $ \dir -> do
  writeFile (dir </> "Aé") ""
  ignoresCase <- doesFileExist (dir </> "aé")
  normalizesUnicode <- doesFileExist (dir </> "Ae\x301")
  let foldCase = if ignoresCase then T.toLower else id
      normalizeText = if normalizesUnicode then Unicode.normalize Unicode.NFC else id
  pure (T.unpack . normalizeText . foldCase . T.pack . normalise)

type Outputs = [(FilePath, FilePath, Bool)] -- output, parent source, public

blockRules :: Bool -> String -> P.ReaderOptions -> P.WriterOptions -> Context String
           -> (String -> Item P.Pandoc -> Compiler (Item P.Pandoc)) -> Rules Outputs
blockRules fixtures root reader writer ctx cite = do
  let sources glob = if fixtures then fromGlob glob .||. fromGlob ("fixtures/" ++ glob) else fromGlob glob
      fields = sources "fields/*/field.yaml"
      blocks = sources "fields/*/*.md"
      courses = sources "courses/*.yaml"
      materials = sources "courses/*/*.md"
      inputs = fields .||. blocks .||. courses .||. materials
      paths pattern = map toFilePath <$> getMatches pattern
  fs <- paths fields
  bs <- paths blocks
  cs <- paths courses
  ms <- paths materials
  site <- preprocess $ readSite reader fs bs cs ms
  coursePlans <- preprocess $ forM (siteCourses site) $ \c -> (,) c <$> planCourseSections site c
  let redirects = [(blockDir b </> section <.> "html", b, section)
                  | b <- M.elems (siteBlocks site), Just section <- [blockMergedSection b]]
      outputs = [(fieldPath f,fieldSource f,True) | f <- siteFields site] ++
                [(pagePath b p,blockSource b,blockOpen b) | b <- M.elems (siteBlocks site), p <- blockPages b] ++
                [(path,blockSource b,False) | (path,b,_) <- redirects] ++
                [(path,courseSource c,courseOpen c) | (c, sections) <- coursePlans,
                   path <- coursePath c : [sectionDestination section | section <- sections,
                                          Supplement _ <- [sectionId section]]] ++
                [(materialPath c m,materialSource m,courseOpen c) | c <- siteCourses site,
                   m <- M.elems (coursePages c)] ++
                [(lessonPath c lesson,courseSource c,courseOpen c && lessonOpen lesson)
                | (c, sections) <- coursePlans, section <- sections, lesson <- sectionLessons section]
  preprocess $ unique "fields/courses" "output path" [(p,src) | (p,src,_) <- outputs]
  match inputs $ version "source" $ compile getResourceBody
  -- A change of order, visibility or target ID may affect pages in other fields.
  -- loadAll records membership changes as well as changes to individual sources.
  create ["block-site-sources"] $ compile $ do
    void (loadAll (inputs .&&. hasVersion "source") :: Compiler [Item String])
    makeItem ("" :: String)
  let register path compiler = create [fromFilePath path] $ do
        route idRoute
        compile $ dependOnSources >> compiler
  forM_ (siteFields site) $ \f -> register (fieldPath f) (renderField ctx site f)
  forM_ redirects $ \(path,b,section) -> register path (renderBlockRedirect b section)
  forM_ (M.elems $ siteBlocks site) $ \b -> forM_ (blockPages b) $ \p ->
    register (pagePath b p) $ do
      body <- if blockOpen b
        then renderDocument (blockBibliography b) (`M.member` blockAnchors b) (pageDoc p)
        else makeItem ""
      renderPage ctx site coursePlans b p body
  forM_ coursePlans $ \(c, sections) -> do
    let render compiler
          | courseOpen c = compiler
          | otherwise = renderClosedCourse ctx c
        navigation = courseNavigation c sections
        lessons = concatMap sectionLessons sections
    register (coursePath c) $ render (renderCourse ctx c navigation)
    forM_ sections $ \section -> case sectionId section of
      Supplement _ -> do
        intro <- preprocess $ either (failAt $ courseSource c) pure $
          sessionDoc reader site c lessons section S.empty
        register (sectionDestination section) $ render $ do
          html <- writePandocWith writer <$> makeItem intro
          renderSupplement ctx site c navigation section html
      Numbered _ -> do
        let inSession = sectionLessons section
        forM_ (zip inSession (map (const False) (drop 1 inSession) ++ [True])) $ \(lesson, lastInSession) -> do
          let path = lessonPath c lesson
          register path $ do
            -- Validate the same rendered page even while its course is closed.
            body <- renderLessonDocument site c lessons lesson
            intro <- if path == sectionDestination section
              then do
                let lessonIds = S.fromList (htmlIds (T.pack (itemBody body)))
                doc <- either (failAt $ courseSource c) pure $
                  sessionDoc reader site c lessons section lessonIds
                itemBody . writePandocWith writer <$> makeItem doc
              else pure ""
            html <- renderLesson root ctx site c navigation section lesson intro lastInSession body
            unique path "HTML ID" [(anchor, path) | anchor <- htmlIds (T.pack (itemBody html))]
            if courseOpen c then pure html else renderClosedCourse ctx c
    forM_ (M.elems $ coursePages c) $ \m ->
      register (materialPath c m) $ do
        body <- if courseOpen c
          then renderDocument (materialBibliography m) (`S.member` materialAnchors m)
                 (walk demoteMaterialHeading (materialDoc m))
          else makeItem ""
        renderMaterial ctx c navigation m body
  pure outputs
  where
    demoteMaterialHeading (P.Header level attrs inlines) = P.Header (min 6 (level + 1)) attrs inlines
    demoteMaterialHeading block = block
    renderDocument bibliography containsId doc =
      writePandocWith writer <$> prepareDocument bibliography containsId doc
    prepareDocument bibliography containsId doc =
      case bibliography of
        Just bib | or (query hasCite doc) -> do
          let candidates = "bibliography" : ["bibliography-" ++ show n | n <- [1 :: Int ..]]
              anchor = head (dropWhile containsId candidates)
          makeItem (appendBibliography anchor doc) >>= cite bib
        _ -> makeItem doc
    hasCite (P.Cite _ _) = [True]
    hasCite _ = []
    appendBibliography anchor (P.Pandoc meta bs) = P.Pandoc meta $
      bs ++ [P.Header 2 (T.pack anchor,[],[]) [P.Str "参考文献"], P.Div ("refs",[],[]) []]

    -- Run citeproc with each block's bibliography, then write all blocks at once
    -- so Pandoc can assign page-wide identifiers to footnotes and code.
    renderLessonDocument site course lessons lesson = do
      let parts = lessonBlocks lesson
          titleIds = map (blockName . lessonBlock) (drop 1 parts)
          reserved = S.fromList (titleIds ++ concatMap partAnchors parts)
      (_, documents) <- foldM (renderPart site course lessons lesson) (reserved, [])
          (zip (False : repeat True) parts)
      let body = concat [blocks | P.Pandoc _ blocks <- reverse documents]
      writePandocWith writer <$> makeItem (P.Pandoc P.nullMeta body)

    renderPart site course lessons lesson (used, documents) (showTitle, part)
      | blockOpen block = do
          original <- either (failAt $ blockSource block) pure $ lessonDocument site course lessons lesson part
          let originalIds = S.fromList (documentIds original)
          cited <- itemBody <$> prepareDocument (blockBibliography block) (`S.member` originalIds) original
          let generated = S.fromList (documentIds cited) S.\\ originalIds
              (nextIds, names) = foldl allocate (used, M.empty) (S.toList generated)
              body = walk (rewriteCitation names) (renameDocumentIds names cited)
          pure (nextIds, prependTitle body : documents)
      | otherwise = pure (used, prependTitle (closedPart showTitle part) : documents)
      where
        block = lessonBlock part
        prependTitle doc@(P.Pandoc meta blocks)
          | showTitle = P.Pandoc meta $
              P.Header 1 (T.pack (blockName block), ["lesson-block-title"], [])
                [P.Str (T.pack (blockTitle block))] : blocks
          | otherwise = doc
        allocate (ids, names) anchor
          | S.notMember anchor ids = (S.insert anchor ids, names)
          | otherwise =
              let base = blockName block ++ "-" ++ anchor
                  candidates = base : [base ++ "-" ++ show n | n <- [1 :: Int ..]]
                  chosen = head (filter (`S.notMember` ids) candidates)
              in (S.insert chosen ids, M.insert anchor chosen names)

    rewriteCitation names (P.Link attrs label (url, title))
      | Just fragment <- stripPrefix "#" (T.unpack url)
      , Just renamed <- M.lookup (unEscapeString fragment) names =
          P.Link attrs label (T.pack (anchorSuffix renamed), title)
    rewriteCitation _ inline = inline

    partAnchors part = map (lessonAnchor part) (M.keys (blockAnchors (lessonBlock part)))
    closedPart showTitle part = P.Pandoc P.nullMeta $
      P.Para (label ++ [P.Span ("", ["badge", "badge-draft"], []) [P.Str "準備中"]])
      : [P.Plain [P.Span (T.pack anchor, [], []) []] | anchor <- partAnchors part]
      where
        label
          | showTitle = []
          | otherwise = [P.Strong [P.Str (T.pack (blockTitle (lessonBlock part)))], P.Space]

dependOnSources :: Compiler ()
dependOnSources = void (loadBody "block-site-sources" :: Compiler String)

sitemapItems :: Configuration -> PathKey -> Outputs -> Compiler [Item String]
sitemapItems conf key outputs = do
  dependOnSources
  items <- forM outputs $ \(path,_,public) -> do
    page <- load (fromFilePath path)
    pure [page | public]
  unsafeCompiler $ forgetRemovedPages conf key [p | (p,_,_) <- outputs]
  pure (concat items)

generatedMtime :: Outputs -> Context String
generatedMtime outputs = field "mtime" $ \item -> do
  let sourceMap = M.fromList [(p,src) | (p,src,_) <- outputs]
  case M.lookup (toFilePath $ itemIdentifier item) sourceMap of
    Nothing -> noResult "existing sitemap entry"
    Just src -> formatTime defaultTimeLocale "%Y-%m-%d" <$> getItemModificationTime (fromFilePath src)

-- Hakyll watch re-registers create rules, but leaves their old output on disk.
-- Only remove paths registered by the previous successful page compilation.
forgetRemovedPages :: Configuration -> PathKey -> [FilePath] -> IO ()
forgetRemovedPages conf key paths = do
  let manifest = storeDirectory conf </> "block-page-routes"
      current = S.fromList paths
      liveKeys = S.map key current
      owned path = takeExtension path == ".html" && case splitDirectories path of
        ds@(root:_) -> root `elem` ["fields","courses"] && all (`notElem` [".",".."]) ds
        _ -> False
  exists <- doesFileExist manifest
  previous <- if exists then do
    contents <- readFile manifest
    maybe (failAt manifest "invalid page list") pure (readMaybe contents)
    else pure S.empty
  forM_ [p | p <- S.toList previous, S.notMember (key p) liveKeys] $ \path -> do
    unless (owned path) $ failAt manifest ("invalid page path " ++ path)
    let output = destinationDirectory conf </> path
    present <- doesFileExist output
    when present (removeFile output)
  when (current /= previous) $ do
    writeFile (manifest <.> "new") (show current)
    renameFile (manifest <.> "new") manifest
