--------------------------------------------------------------------------------
{-# LANGUAGE Arrows             #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main (main) where


--------------------------------------------------------------------------------
import           Control.Monad   ((>=>))
import           Prelude
import           Data.Functor.Identity (runIdentity)
import           System.Exit     (ExitCode)
import           System.FilePath (replaceExtension, takeBaseName, takeDirectory, takeFileName)
import qualified Data.Text as T
import qualified System.Process  as Process
import qualified Text.Pandoc     as Pandoc
import qualified Data.Set as S
import Control.Monad (foldM, mplus)
import Data.List (elemIndex, find, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Hakyll

import qualified Data.Map as M
import Text.Pandoc.Highlighting

--------------------------------------------------------------------------------
-- KaTeX context
mathCtx :: Context a
mathCtx = field "katex" $ \item -> do
  katex <- getMetadataField (itemIdentifier item) "katex"
  return $ case katex of
    Just "false" -> ""
    Just "off"   -> ""
    _ ->
      "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.0/dist/katex.min.css\" integrity=\"sha384-Xi8rHCmBmhbuyyhbI88391ZKP2dmfnOl4rT9ZfRI7mLTdk1wblIUnrIq35nqwEvC\" crossorigin=\"anonymous\">\n\
      \<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.0/dist/katex.min.js\" integrity=\"sha384-X/XCfMm41VSsqRNQgDerQczD69XqmjOOOwYQvr/uuC+j4OPoNhVgjdGFwhvN02Ja\" crossorigin=\"anonymous\"></script>\n\
      \<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.0/dist/contrib/auto-render.min.js\" integrity=\"sha384-+XBljXPPiv+OzfbB3cVmLHf4hdUFHlWNZN5spNQ7rmHTXpd7WvJum6fIACpNNfIR\" crossorigin=\"anonymous\" onload=\"renderMathInElement(document.body);\"></script>"
--------------------------------------------------------------------------------
-- Pandoc styling
pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

--------------------------------------------------------------------------------
-- Custom ReaderOptions: disable `$...$` math, enable fenced_divs, plus TOC etc.

customReaderOptions :: Pandoc.ReaderOptions
customReaderOptions =
  defaultHakyllReaderOptions

--------------------------------------------------------------------------------
-- Custom WriterOptions: disable `$...$` math, enable fenced_divs, plus TOC etc.
customWriterOptions :: Pandoc.WriterOptions
customWriterOptions = defaultHakyllWriterOptions
  { Pandoc.writerNumberSections  = False
  , Pandoc.writerTableOfContents = False
  , Pandoc.writerTOCDepth        = 3
  , Pandoc.writerTemplate        = Nothing
  , Pandoc.writerHTMLMathMethod  = Pandoc.KaTeX ""
  , Pandoc.writerExtensions      = Pandoc.enableExtension Pandoc.Ext_fenced_divs
                                 $ Pandoc.enableExtension Pandoc.Ext_tex_math_dollars
                                 $ Pandoc.enableExtension Pandoc.Ext_tex_math_double_backslash
                                 $ Pandoc.enableExtension Pandoc.Ext_tex_math_single_backslash
                                 $ Pandoc.pandocExtensions
  }

--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = do
  now <- getPOSIXTime
  let cssVersion = show (round now :: Integer)
      versionCtx = constField "cssVersion" cssVersion
  hakyllWith config $ do
    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ styleToCss pandocCodeStyle

    -- Static files
    match ("images/**.jpg" .||. "images/**.png" .||. "images/**.gif" .||.
            "images/**.mp4" .||.
            "favicon.ico" .||. "files/**" .||. "slds_papers/**.pdf" .||.
            "slds_data/**" .||. "slds_code/**") $ do
        route   idRoute
        compile copyFileCompiler

    -- Formula images
    match "images/*.tex" $ do
        route   $ setExtension "png"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/formula.tex" defaultContext
            >>= xelatex >>= pdfToPng

    -- Dot images
    match "images/*.dot" $ do
        route   $ setExtension "png"
        compile $ getResourceLBS >>= traverse (unixFilterLBS "dot" ["-Tpng"])

    -- Compress CSS into one file.
    match "css/*" $ compile compressCssCompiler
    create ["style.css"] $ do
        route idRoute
        compile $ do
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody csses

    -- Render the /tmp index page
    match "tmp/index.html" $ do
        route idRoute
        compile $ getResourceBody >>= relativizeUrls

    -- Build tags
    tags <- buildTagsWithList ["posts/*","lectures/**"] (fromCapture "tags/*.html")


    -- Render each and every post
    ------------------------------------------------------------------
    match ("posts/*.md" .||. "posts/*.html" .||. "posts/*.lhs") $ do
        route   $ setExtension ".html"
        compile $ do
                underlying <- getUnderlying
                prev       <- getMetadataField underlying "previousChapter"
                next       <- getMetadataField underlying "nextChapter"
                date       <- getMetadataField underlying "date"
                open       <- isOpen underlying

                let postCtxWithChapters = postCtx tags <>
                        field "previousChapter" (\_ -> return $ maybe "#" toUrl prev) <>
                        field "nextChapter" (\_ -> return $ maybe "#" toUrl next) <>
                        field "date" (\_ -> return $ fromMaybe "No Date" date) <>
                        constField "category" "Blog"

                if open
                  then pandocCompilerWith customReaderOptions customWriterOptions
                        >>= saveSnapshot "content"
                        >>= return . fmap demoteHeaders
                        >>= loadAndApplyTemplate "templates/post.html" postCtxWithChapters
                        >>= loadAndApplyTemplate "templates/content.html" (defaultContext)
                        >>= loadAndApplyTemplate "templates/default.html" (versionCtx <> mathCtx <> defaultContext <> constField "post" "true")
                        >>= relativizeUrls
                  else makeItem ""
                        >>= saveSnapshot "content"
                        >>= loadAndApplyTemplate "templates/placeholder.html" postCtxWithChapters
                        >>= loadAndApplyTemplate "templates/content.html" defaultContext
                        >>= loadAndApplyTemplate "templates/default.html"
                                (versionCtx <> defaultContext
                                 <> constField "post" "true"
                                 <> constField "noindex" "true")
                        >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            -- keep closed items visible but marked "準備中"
            let ctx = versionCtx <>
                        constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = versionCtx <>
                        constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        -- Create RSS feed as well
        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= filterOpen
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration title) feedCtx


    -- Render each and every lecture
    ------------------------------------------------------------------
    match ("lectures/**.md" .||. "lectures/**.html" .||. "lectures/**.lhs") $ do
        route   $ customRoute $ \ident ->
            "lectures/" ++ replaceExtension (takeFileName (toFilePath ident)) "html"
        compile $ do
            underlying <- getUnderlying
            prev       <- getMetadataField underlying "previousChapter"
            next       <- getMetadataField underlying "nextChapter"
            date       <- getMetadataField underlying "date"
            prevTitle  <- getChapterTitle prev
            nextTitle  <- getChapterTitle next
            series     <- buildSeries underlying
            open       <- isOpen underlying
            let seriesHtml = renderSeriesHtml underlying series

            let postCtxWithChapters = postCtx tags <>
                  field "previousChapter" (\_ -> maybe (noResult "no prev") (return . toUrl) prev) <>
                  field "nextChapter" (\_ -> maybe (noResult "no next") (return . toUrl) next) <>
                  field "previousChapterTitle" (\_ -> return prevTitle) <>
                  field "nextChapterTitle" (\_ -> return nextTitle) <>
                  field "date" (\_ -> return $ fromMaybe "No Date" date) <>
                  constField "category" "Lecture"
            if open
              then pandocCompilerWith customReaderOptions customWriterOptions
                    >>= saveSnapshot "content"
                    >>= return . fmap demoteHeaders
                    >>= loadAndApplyTemplate "templates/lecture.html" postCtxWithChapters
                    >>= loadAndApplyTemplate "templates/content.html" (defaultContext )
                    >>= loadAndApplyTemplate "templates/default.html"
                            (versionCtx <> mathCtx <> defaultContext
                             <> constField "lecture" "true"
                             <> constField "seriesChapters" seriesHtml)
                    >>= relativizeUrls
              else makeItem ""
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate "templates/placeholder.html" postCtxWithChapters
                    >>= loadAndApplyTemplate "templates/content.html" defaultContext
                    >>= loadAndApplyTemplate "templates/default.html"
                            (versionCtx <> defaultContext
                             <> constField "lecture" "true"
                             <> constField "seriesChapters" seriesHtml
                             <> constField "noindex" "true")
                    >>= relativizeUrls


    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Lectures tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            lectures <- recentFirst =<< loadAll pattern
            let ctx = versionCtx <>
                        constField "title" title <>
                        listField "lectures" (postCtx tags) (return lectures) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/lectures.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        -- Create RSS feed as well
        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= filterOpen
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration title) feedCtx


    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentByMtime =<< featured =<< filterOpen =<< loadAll "posts/*"
            lectures <- fmap (take 5) (recentByMtime =<< featured =<< filterOpen =<< loadAll "lectures/**")
            let indexContext =
                    versionCtx <>
                    listField "posts" (postCtx tags) (return (posts)) <>
                    listField "lectures" (postCtx tags) (return lectures) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext



            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/content.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

    -- Render some static pages
    match (fromList pages) $ do
        route   $ setExtension ".html"
        compile $ pandocCompilerWith customReaderOptions customWriterOptions
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" (versionCtx <> defaultContext)
            >>= relativizeUrls

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompilerWith customReaderOptions customWriterOptions
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" (versionCtx <> defaultContext)

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= filterOpen
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration "All posts") feedCtx

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- filterOpen =<< loadAllSnapshots "posts/*" "content"
            lectures <- filterOpen =<< loadAllSnapshots "lectures/**" "content"
            let allPosts = posts ++ lectures
            let sitemapCtx = constField "root" "https://yakagika.github.io" <>
                             listField "entries" (postCtx tags) (return allPosts)

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                >>= relativizeUrls


    -- Showcases
    match "photos/*/index.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= relativizeUrls
    match "photos/*/*.jpg" $ do
        route idRoute
        compile copyFileCompiler
    -- google
    match "google444b04943c3fafb4.html" $ do
        route idRoute
        compile copyFileCompiler

  where
    pages =
        [ "contact.markdown"
        , "research.markdown"
        , "links.markdown"
        , "lectures.markdown"
        , "slds_papers.markdown"
        ]

    writeXeTex :: Item Pandoc.Pandoc -> Compiler (Item String)
    writeXeTex = traverse $ \pandoc ->
        case Pandoc.runPure (Pandoc.writeLaTeX Pandoc.def pandoc) of
            Left err -> fail $ show err
            Right x  -> return (T.unpack x)

------------------------------------------------------------------
buildTagsWithList :: MonadMetadata m => [Pattern] -> (String -> Identifier) -> m Tags
buildTagsWithList patterns makeId = do
    ids <- concat <$> mapM getMatches patterns
    tagMap <- foldM addTags M.empty ids
    let set' = S.fromList ids
    return $ Tags (M.toList tagMap) makeId (PatternDependency (mconcat patterns) set')
  where
    -- Create a tag map for one page
    addTags tagMap id' = do
        tags <- getTags id'
        let tagMap' = M.fromList $ zip tags $ repeat [id']
        return $ M.unionWith (++) tagMap tagMap'

--------------------------------------------------------------------------------
-- | An entry is open (published) unless its metadata says `open: false`.
-- Missing `open` key defaults to true (backwards compatible).
isOpen :: MonadMetadata m => Identifier -> m Bool
isOpen ident = do
    v <- getMetadataField ident "open"
    return $ case v of
        Just "false" -> False
        Just "False" -> False
        Just "no"    -> False
        _            -> True

--------------------------------------------------------------------------------
filterOpen :: MonadMetadata m => [Item a] -> m [Item a]
filterOpen = filterM (isOpen . itemIdentifier)

--------------------------------------------------------------------------------
-- | boolField wrapper: shows contents when predicate is true, else empty.
openField :: String -> Context a
openField name = field name $ \item -> do
    o <- isOpen (itemIdentifier item)
    if o then return "" else noResult "closed"

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%Y-%m-%d"
    , dateField "date" "%Y-%m-%d"
    , tagsField "tags" tags
    , urlField "url"
    , openField "isOpen"
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]


--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  , previewHost = "0.0.0.0"
  }

--------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "yakagika - " ++ title
    , feedDescription = "Personal blog of yakagika"
    , feedAuthorName  = "yakagika"
    , feedAuthorEmail = "kaya3728@gmail.com"
    , feedRoot        = ""
    }


--------------------------------------------------------------------------------
-- | Hacky.
xelatex :: Item String -> Compiler (Item TmpFile)
xelatex item = do
    TmpFile texPath <- newTmpFile "xelatex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <- Process.system $ unwords ["xelatex", "-halt-on-error",
            "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
        return ()

    makeItem $ TmpFile pdfPath


--------------------------------------------------------------------------------
pdfToPng :: Item TmpFile -> Compiler (Item TmpFile)
pdfToPng item = do
    let TmpFile pdfPath = itemBody item
        pngPath         = replaceExtension pdfPath "png"
    unsafeCompiler $ do
        _ <- Process.system $ unwords
            ["convert", "-density", "150", "-quality", "90", pdfPath, pngPath]
        return ()
    makeItem $ TmpFile pngPath


--------------------------------------------------------------------------------
photographCtx :: Context String
photographCtx = mconcat
    [ dateField "date" "%Y-%m-%d"
    , metadataField
    ]


--------------------------------------------------------------------------------
featured :: MonadMetadata m => [Item a] -> m [Item a]
featured = filterM $ \item -> do
    val <- getMetadataField (itemIdentifier item) "featured"
    pure $ val == Just "true"


--------------------------------------------------------------------------------
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p = mapM (\x -> (,) x <$> p x) >=> pure . map fst . filter snd

--------------------------------------------------------------------------------
-- | Look up the title of a chapter by its filename (e.g. "iap1.html" -> "Ch1 ...")
getChapterTitle :: Maybe String -> Compiler String
getChapterTitle Nothing     = return ""
getChapterTitle (Just file) = do
    mIdent <- chapterFileToIdentifier file
    case mIdent of
        Nothing    -> return ""
        Just ident -> fromMaybe "" <$> getMetadataField ident "title"

--------------------------------------------------------------------------------
-- | Resolve a chapter filename (e.g. "iap1.html") to the lecture source
-- Identifier, searching all subfolders under lectures/ by basename.
chapterFileToIdentifier :: String -> Compiler (Maybe Identifier)
chapterFileToIdentifier file = do
    let baseName = takeBaseName file
    matches <- getMatches "lectures/**.md"
    return $ find (\i -> takeBaseName (toFilePath i) == baseName) matches

--------------------------------------------------------------------------------
-- | Walk backward via previousChapter to find the first chapter of a series
-- Stops if the referenced chapter file doesn't exist
findFirstChapter :: Identifier -> Compiler Identifier
findFirstChapter ident = do
    prev <- getMetadataField ident "previousChapter"
    case prev of
        Nothing -> return ident
        Just p  -> do
            mPrevIdent <- chapterFileToIdentifier p
            case mPrevIdent of
                Nothing        -> return ident
                Just prevIdent -> do
                    prevTitle <- getMetadataField prevIdent "title"
                    case prevTitle of
                        Nothing -> return ident  -- broken link
                        Just _  -> findFirstChapter prevIdent

--------------------------------------------------------------------------------
-- | Walk forward via nextChapter collecting (filename, title, identifier)
-- Stops if the referenced chapter doesn't exist (no title found)
collectForward :: Identifier -> Compiler [(String, String, Identifier, Bool)]
collectForward ident = do
    title <- getMetadataField ident "title"
    case title of
        Nothing -> return []  -- chapter file doesn't exist
        Just t  -> do
            let file    = toFilePath ident
                urlFile = replaceExtension (takeFileName file) "html"
            open <- isOpen ident
            next <- getMetadataField ident "nextChapter"
            rest <- case next of
                Nothing -> return []
                Just n  -> do
                    mNextIdent <- chapterFileToIdentifier n
                    case mNextIdent of
                        Nothing        -> return []
                        Just nextIdent -> collectForward nextIdent
            return $ (urlFile, t, ident, open) : rest

--------------------------------------------------------------------------------
-- | Build the full chapter series containing the given lecture
buildSeries :: Identifier -> Compiler [(String, String, Identifier, Bool)]
buildSeries ident = do
    start <- findFirstChapter ident
    collectForward start

--------------------------------------------------------------------------------
-- | Render the chapter series as an HTML list with current chapter marked
renderSeriesHtml :: Identifier -> [(String, String, Identifier, Bool)] -> String
renderSeriesHtml curIdent chapters =
    "<ul class=\"toc-root\">" ++
    concatMap renderOne chapters ++
    "</ul>"
  where
    renderOne (url, title, ident, open) =
        let currentClass = if ident == curIdent then " current" else ""
            draftClass   = if open then "" else " draft"
            label        = escapeHtmlChars title
            link         = if open
                             then "<a href=\"" ++ url ++ "\">" ++ label ++ "</a>"
                             else "<span class=\"draft-title\">" ++ label ++
                                  "</span> <span class=\"badge badge-draft\">準備中</span>"
        in "<li class=\"toc-item toc-series-item" ++ currentClass ++ draftClass ++ "\">" ++
           "<div class=\"toc-row\">" ++
           "<span class=\"toc-toggle\">▾</span>" ++
           link ++
           "</div>" ++
           "<ul class=\"toc-sublist\"></ul>" ++
           "</li>"

escapeHtmlChars :: String -> String
escapeHtmlChars = concatMap f
  where
    f '<' = "&lt;"
    f '>' = "&gt;"
    f '&' = "&amp;"
    f '"' = "&quot;"
    f c   = [c]

--------------------------------------------------------------------------------
-- | Sort items by file modification time (most recent first)
recentByMtime :: [Item a] -> Compiler [Item a]
recentByMtime items = do
    itemsWithTime <- mapM (\item -> do
        time <- getItemModificationTime (itemIdentifier item)
        return (item, time)) items
    return $ map fst $ sortBy (\(_, t1) (_, t2) -> compare (Down t1) (Down t2)) itemsWithTime
