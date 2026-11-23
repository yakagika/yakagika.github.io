{-# LANGUAGE OverloadedStrings #-}
module BlockSite (checkedRoutes, blockRules, sitemapItems, generatedMtime) where

import BlockModel
import BlockViews
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Normalize as Unicode
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hakyll
import Hakyll.Core.Rules.Internal (Rules(..), RuleSet(..), rulesProvider)
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

blockRules :: Bool -> P.ReaderOptions -> P.WriterOptions -> Context String
           -> (String -> Item P.Pandoc -> Compiler (Item P.Pandoc)) -> Rules Outputs
blockRules fixtures reader writer ctx cite = do
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
  let outputs = [(fieldPath f,fieldSource f,True) | f <- siteFields site] ++
                [(pagePath b p,blockSource b,blockOpen b) | b <- M.elems (siteBlocks site), p <- blockPages b] ++
                [(path,courseSource c,courseOpen c) | c <- siteCourses site,
                   path <- coursePath c : map (sectionPath c . fst) (courseSections c)] ++
                [(materialPath c m,materialSource m,courseOpen c) | c <- siteCourses site,
                   m <- M.elems (coursePages c)]
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
  forM_ (M.elems $ siteBlocks site) $ \b -> forM_ (blockPages b) $ \p ->
    register (pagePath b p) $ do
      body <- if blockOpen b
        then renderDocument (blockBibliography b) (`M.member` blockAnchors b) (pageDoc p)
        else makeItem ""
      renderPage ctx site b p body
  forM_ (siteCourses site) $ \c -> do
    let render compiler
          | courseOpen c = compiler
          | otherwise = renderClosedCourse ctx c
    register (coursePath c) $ render (renderCourse ctx c)
    forM_ (courseSections c) $ \(key,s) -> do
      intro <- preprocess $ either (failAt $ courseSource c) pure $ sessionDoc reader site c s
      register (sectionPath c key) $ render $ do
        html <- writePandocWith writer <$> makeItem intro
        renderSession ctx site c key s html
    forM_ (M.elems $ coursePages c) $ \m ->
      register (materialPath c m) $ do
        body <- if courseOpen c
          then renderDocument (materialBibliography m) (`S.member` materialAnchors m)
                 (walk demoteMaterialHeading (materialDoc m))
          else makeItem ""
        renderMaterial ctx c m body
  pure outputs
  where
    demoteMaterialHeading (P.Header level attrs inlines) = P.Header (min 6 (level + 1)) attrs inlines
    demoteMaterialHeading block = block
    renderDocument bibliography containsId doc = do
      rendered <- case bibliography of
        Just bib | or (query hasCite doc) -> do
          let candidates = "bibliography" : ["bibliography-" ++ show n | n <- [1 :: Int ..]]
              anchor = head (dropWhile containsId candidates)
          makeItem (appendBibliography anchor doc) >>= cite bib
        _ -> makeItem doc
      pure $ writePandocWith writer rendered
    hasCite (P.Cite _ _) = [True]
    hasCite _ = []
    appendBibliography anchor (P.Pandoc meta bs) = P.Pandoc meta $
      bs ++ [P.Header 2 (T.pack anchor,[],[]) [P.Str "参考文献"], P.Div ("refs",[],[]) []]

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
