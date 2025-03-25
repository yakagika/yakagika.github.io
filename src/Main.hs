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
import           System.FilePath (replaceExtension, takeDirectory)
import qualified Data.Text as T
import qualified System.Process  as Process
import qualified Text.Pandoc     as Pandoc
import qualified Data.Set as S
import Control.Monad (foldM, mplus)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
import           Hakyll

import qualified Data.Map as M
import Text.Pandoc.Highlighting
mathCtx :: Context a
mathCtx = field "katex" $ \item -> do
    katex <- getMetadataField (itemIdentifier item) "katex"
    return $ case katex of
                    Just "false" -> ""
                    Just "off" -> ""
                    _ -> "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/katex.min.css\" integrity=\"sha384-wcIxkf4k558AjM3Yz3BBFQUbk/zgIYC2R0QpeeYb+TwlBVMrlgLqwRjRtGZiK7ww\" crossorigin=\"anonymous\">\n\
                             \<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/katex.min.js\" integrity=\"sha384-hIoBPJpTUs74ddyc4bFZSM1TVlQDA60VBbJS0oA934VSz82sBx1X7kSx2ATBDIyd\" crossorigin=\"anonymous\"></script>\n\
                             \<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/contrib/auto-render.min.js\" integrity=\"sha384-43gviWU0YVjaDtb/GhzOouOXtZMP/7XUzwPTstBeZFe/+rCMvRwr4yROQP43s0Xk\" crossorigin=\"anonymous\" onload=\"renderMathInElement(document.body);\"></script>"


pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

tocTemplate = either error Prelude.id . runIdentity . Pandoc.compileTemplate "" $ T.unlines
  [ "<div class=\"toc\"><div class=\"header\">Table of Contents</div>"
  , "$toc$"
  , "</div>"
  , "$body$"
  ]

customWriterOptions:: Pandoc.WriterOptions
customWriterOptions = defaultHakyllWriterOptions
                        { Pandoc.writerHTMLMathMethod   = Pandoc.MathJax ""       -- LaTeX
                        , Pandoc.writerNumberSections   = True
                        , Pandoc.writerHighlightStyle   = Just pandocCodeStyle    -- Syntax
                        , Pandoc.writerTableOfContents  = True                    -- toc
                        , Pandoc.writerTOCDepth         = 3
                        , Pandoc.writerTemplate         = Just tocTemplate
                        , Pandoc.writerExtensions       = Pandoc.enableExtension Pandoc.Ext_fenced_divs Pandoc.pandocExtensions
                        }


myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
    pandocCompilerWith defaultHakyllReaderOptions customWriterOptions

--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = hakyllWith config $ do
    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ styleToCss pandocCodeStyle

    -- Static files
    match ("images/*.jpg" .||. "images/*.png" .||. "images/*.gif" .||.
            "images/*.mp4" .||.
            "favicon.ico" .||. "files/**") $ do
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
    tags <- buildTagsWithList ["posts/*","lectures/*"] (fromCapture "tags/*.html")


    -- Render each and every post
    ------------------------------------------------------------------
    match ("posts/*.md" .||. "posts/*.html" .||. "posts/*.lhs") $ do
        route   $ setExtension ".html"
        compile $ do
                underlying <- getUnderlying
                toc        <- getMetadataField underlying "tableOfContents"
                let writerOptions' = maybe defaultHakyllWriterOptions (const customWriterOptions) toc

                pandocCompilerWith defaultHakyllReaderOptions writerOptions'
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" (mathCtx <> defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
                >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = constField "title" "Posts" <>
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
            let ctx = constField "title" title <>
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
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration title) feedCtx


    -- Render each and every lecture
    ------------------------------------------------------------------
    match ("lectures/*.md" .||. "lectures/*.html" .||. "lectures/*.lhs") $ do
        route   $ setExtension ".html"
        compile $ do
            underlying <- getUnderlying
            prev       <- getMetadataField underlying "previousChapter"
            next       <- getMetadataField underlying "nextChapter"
            date       <- getMetadataField underlying "date"

            -- Always use the default writer options (i.e. no built-in TOC)
            let writerOptions' = defaultHakyllWriterOptions
            let postCtxWithChapters = postCtx tags <>
                  field "previousChapter" (\_ -> return $ maybe "#" toUrl prev) <>
                  field "nextChapter" (\_ -> return $ maybe "#" toUrl next) <>
                  field "date" (\_ -> return $ fromMaybe "No Date" date)
            pandocCompilerWith defaultHakyllReaderOptions writerOptions'
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/lecture.html" postCtxWithChapters
                >>= loadAndApplyTemplate "templates/content.html" (mathCtx <> defaultContext)
                -- Add a flag "lecture" so default.html can render the TOC placeholder
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext <> constField "lecture" "true")
                >>= relativizeUrls
                >>= relativizeUrls


    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Lectures tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            lectures <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
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
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration title) feedCtx


    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< featured =<< loadAll "posts/*"
            lectures <- recentFirst =<< featured =<< loadAll "lectures/*"
            let indexContext =
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
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration "All posts") feedCtx

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "content"
            lectures <- loadAllSnapshots "lectures/*" "content"
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
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%Y-%m-%d"
    , dateField "date" "%Y-%m-%d"
    , tagsField "tags" tags
    , urlField "url"
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
