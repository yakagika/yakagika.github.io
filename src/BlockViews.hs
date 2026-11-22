{-# LANGUAGE OverloadedStrings #-}
module BlockViews (renderField, renderCourse, renderSession, renderClosedCourse, renderPage) where

import BlockModel
import Data.List (find, nub)
import qualified Data.Map.Strict as Map
import Hakyll
import Network.URI (escapeURIString, isUnreserved)
import System.FilePath ((</>), (<.>))

renderField :: Context String -> Site -> Field -> Compiler (Item String)
renderField siteCtx site fieldData = do
    let blocks = map (blockNamed site) (fieldBlocks fieldData)
        prerequisites = filter ((/= fieldName fieldData) . blockField)
            (map (blockNamed site) (nub (concatMap blockRequires blocks)))
        listedBlock = blockLinkContext
            <> rows "pages" (linkContext (navigationPageTitle . snd) (uncurry pagePath))
                (\b -> [(b, p) | p <- blockPages b])
        ctx = titleAndDescription (fieldTitle fieldData) (fieldDescription fieldData)
            <> boolField "hasPrerequisites" (const (not (null prerequisites)))
            <> rows "prerequisites" blockLinkContext (const prerequisites)
            <> rows "blocks" listedBlock (const blocks)
    makeItem "" >>= layout "templates/block-field.html" ctx siteCtx

renderCourse :: Context String -> Course -> Compiler (Item String)
renderCourse siteCtx course = do
    let sessions = zip [1 ..] (courseSessions course)
        sessionCtx = linkContext (sessionTitle . snd) (sessionPath course . fst)
        ctx = titleAndDescription (courseTitle course) (courseDescription course)
            <> rows "sessions" sessionCtx (const sessions)
    makeItem "" >>= layout "templates/block-course.html" ctx siteCtx

renderClosedCourse :: Context String -> Course -> Compiler (Item String)
renderClosedCourse siteCtx course = makeItem ""
    >>= layout "templates/placeholder.html" ctx (constField "noindex" "true" <> siteCtx)
  where
    ctx = titleAndDescription (courseTitle course) (courseDescription course)
        <> constField "category" "Lecture"

renderSession :: Context String -> Site -> Course -> Int -> Session -> Item String -> Compiler (Item String)
renderSession siteCtx site course _ session = layout "templates/block-session.html" ctx siteCtx
  where
    ctx = titleAndDescription (sessionTitle session) ""
        <> constField "courseTitle" (escapeHtml (courseTitle course))
        <> constField "courseUrl" (siteUrl (coursePath course))
        <> boolField "hasBlocks" (const (not (null (sessionBlocks session))))
        <> rows "blocks" entryContext (const [(blockNamed site (entryBlock e), e) | e <- sessionBlocks session])
    entryContext = linkContext (blockTitle . fst) (blockPath . fst)
        <> boolField "draft" (not . blockOpen . fst . itemBody)
        <> value "level" (levelLabel . entryLevel . snd)
        <> value "kind" (kindLabel . blockKind . fst)
        <> boolField "hasExercises" (not . null . entryExercises . snd . itemBody)
        <> rows "exercises" exerciseContext (\(b, e) -> [(b, x) | x <- entryExercises e])
    exerciseContext = value "label" exerciseTitle
        <> field "url" (pure . uncurry exerciseUrl . itemBody)
        <> optionalValue "file" (exerciseFile . snd)
    exerciseTitle (b, x) = blockExercises b Map.! exerciseId x

renderPage :: Context String -> Site -> Block -> Page -> Item String -> Compiler (Item String)
renderPage siteCtx site block page body = do
    fieldData <- maybe (fail ("Missing field for block: " ++ blockName block)) pure
        (find ((== blockField block) . fieldName) (siteFields site))
    let currentPage = find ((== pageKey page) . pageKey . middle) (adjacent (blockPages block))
        anchors = [anchor | (anchor, key) <- Map.toList (blockAnchors block), key == pageKey page]
        occurrences = courseOccurrences site block
        current = boolField "current" ((== blockName block) . blockName . itemBody)
        pageNavigation = boolField "hasPageNavigation" (const (length (blockPages block) > 1))
            <> neighborContext navigationPageTitle (pagePath block) (const False)
            (const (currentPage >>= previous)) (const (currentPage >>= next))
        ctx = titleAndDescription (pageTitle page) ""
            <> constField "fieldTitle" (escapeHtml (fieldTitle fieldData))
            <> constField "fieldUrl" (siteUrl (fieldPath fieldData))
            <> boolField "open" (const (blockOpen block))
            <> boolField "introPage" (const (pageKey page == "index"))
            <> rows "fieldBlocks" (blockLinkContext <> current)
                (const (map (blockNamed site) (fieldBlocks fieldData)))
            <> boolField "hasOccurrences" (const (not (null occurrences)))
            <> rows "occurrences" (occurrenceContext site) (const occurrences)
            <> rows "anchors" (value "anchor" id) (const anchors)
            <> pageNavigation
    if blockOpen block
        then layout "templates/block-page.html" ctx (constField "post" "true" <> siteCtx) body
        else loadAndApplyTemplate "templates/placeholder.html"
                (constField "category" "Lecture" <> ctx) (fmap (const "") body)
            >>= layout "templates/block-page.html" ctx (constField "noindex" "true" <> siteCtx)

type Position = (Int, Entry)
type Neighbors a = (Maybe a, a, Maybe a)

courseOccurrences :: Site -> Block -> [(Course, Neighbors Position)]
courseOccurrences site block =
    [ (course, place)
    | course <- siteCourses site
    , courseOpen course
    , place <- adjacent [(n, entry) | (n, session) <- zip [1 ..] (courseSessions course), entry <- sessionBlocks session]
    , entryBlock (snd (middle place)) == blockName block
    ]

occurrenceContext :: Site -> Context (Course, Neighbors Position)
occurrenceContext site =
    value "courseTitle" (courseTitle . fst)
    <> field "courseUrl" (pure . siteUrl . coursePath . fst . itemBody)
    <> value "sessionLabel" (sessionLabel . fst . middle . snd)
    <> field "sessionUrl" (\item -> let (course, place) = itemBody item
                                    in pure (siteUrl (sessionPath course (fst (middle place)))))
    <> value "level" (levelLabel . entryLevel . snd . middle . snd)
    <> neighborContext (blockTitle . positionBlock) (blockPath . positionBlock)
        (not . blockOpen . positionBlock) (previous . snd) (next . snd)
  where
    positionBlock = blockNamed site . entryBlock . snd

neighborContext :: (a -> String) -> (a -> FilePath) -> (a -> Bool)
    -> (b -> Maybe a) -> (b -> Maybe a) -> Context b
neighborContext linkTitle linkPath draft prev after =
    neighbor "previous" prev <> neighbor "next" after
  where
    neighbor prefix select =
        optionalValue (prefix ++ "Url") (fmap (toUrl . linkPath) . select)
        <> optionalValue (prefix ++ "Title") (fmap linkTitle . select)
        <> boolField (prefix ++ "Draft") (maybe False draft . select . itemBody)

blockNamed :: Site -> String -> Block
blockNamed site name = siteBlocks site Map.! name

blockLinkContext :: Context Block
blockLinkContext = linkContext blockTitle blockPath <> boolField "draft" (not . blockOpen . itemBody)

linkContext :: (a -> String) -> (a -> FilePath) -> Context a
linkContext getTitle getPath = value "label" getTitle <> field "url" (pure . siteUrl . getPath . itemBody)

exerciseUrl :: Block -> Exercise -> String
exerciseUrl block exercise = siteUrl (blockDir block </> key <.> "html") ++ "#" ++ escapeURIString isUnreserved anchor
  where
    anchor = exerciseId exercise
    key = blockAnchors block Map.! anchor

siteUrl :: FilePath -> String
siteUrl = escapeHtml . toUrl

navigationPageTitle :: Page -> String
navigationPageTitle page
    | pageKey page == "index" = "導入"
    | otherwise = pageTitle page

sessionLabel :: Int -> String
sessionLabel number = "第 " ++ show number ++ " 回"

levelLabel :: String -> String
levelLabel "required" = "必修"
levelLabel "advanced" = "発展"
levelLabel level = level

kindLabel :: String -> String
kindLabel "theory" = "理論"
kindLabel "code" = "コード"
kindLabel kind = kind

titleAndDescription :: String -> String -> Context a
titleAndDescription title description = constField "title" (escapeHtml title)
    <> optionalValue "description" (const (if null description then Nothing else Just description))

value :: String -> (a -> String) -> Context a
value key select = field key (pure . escapeHtml . select . itemBody)

optionalValue :: String -> (a -> Maybe String) -> Context a
optionalValue key select = field key (maybe (noResult key) (pure . escapeHtml) . select . itemBody)

rows :: String -> Context a -> (b -> [a]) -> Context b
rows key ctx select = listFieldWith key ctx (\item -> pure [fmap (const row) item | row <- select (itemBody item)])

layout :: Identifier -> Context String -> Context String -> Item String -> Compiler (Item String)
layout template ctx siteCtx item =
    loadAndApplyTemplate template (ctx <> bodyField "body") item
    >>= loadAndApplyTemplate "templates/content.html" (bodyField "body")
    >>= loadAndApplyTemplate "templates/default.html" (ctx <> siteCtx <> bodyField "body")
    >>= relativizeUrls

adjacent :: [a] -> [Neighbors a]
adjacent items = zip3 (Nothing : map Just items) items (map Just (drop 1 items) ++ [Nothing])

previous :: Neighbors a -> Maybe a
previous (before, _, _) = before

middle :: Neighbors a -> a
middle (_, item, _) = item

next :: Neighbors a -> Maybe a
next (_, _, after) = after
