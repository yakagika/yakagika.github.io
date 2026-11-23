{-# LANGUAGE OverloadedStrings #-}
module BlockViews (renderField, renderCourse, renderSession, renderClosedCourse, renderMaterial, renderPage) where

import BlockModel
import Data.List (find, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Hakyll
import Network.URI (escapeURIString, isUnreserved)
import System.FilePath ((</>), (<.>))
import qualified Text.Pandoc as P
import Text.Pandoc.Shared (stringify)

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
    makeItem "" >>= layout "templates/block-field.html" (ctx <> fieldIndex fieldData blocks Nothing) siteCtx

fieldIndex :: Field -> [Block] -> Maybe (Block, Page) -> Context a
fieldIndex fieldData blocks current =
    boolField "fieldIndex" (const True)
    <> boolField "fieldIndexCurrent" (const (maybe True (const False) current))
    <> constField "fieldTitle" (escapeHtml (fieldTitle fieldData))
    <> constField "fieldUrl" (siteUrl (fieldPath fieldData))
    <> rows "fieldBlocks" blockContext (const blocks)
  where
    blockContext = blockLinkContext
        <> boolField "current" (isCurrentBlock . itemBody)
        <> rows "pages" pageContext (\b -> [(b, p) | p <- blockPages b])
    pageContext = linkContext (navigationPageTitle . snd) (uncurry pagePath)
        <> boolField "current" (\item -> currentPath == Just (uncurry pagePath (itemBody item)))
    isCurrentBlock b = Just (blockName b) == fmap (blockName . fst) current
    currentPath = fmap (uncurry pagePath) current

renderCourse :: Context String -> Course -> Compiler (Item String)
renderCourse siteCtx course = do
    let (sessions, supplements) = splitAt (length (courseSessions course)) (courseSections course)
        sessionCtx = linkContext (sessionTitle . snd) (sectionPath course . fst)
        ctx = titleAndDescription (courseTitle course) (courseDescription course)
            <> rows "sessions" sessionCtx (const sessions)
            <> boolField "hasSupplements" (const (not (null supplements)))
            <> rows "supplements" sessionCtx (const supplements)
            <> materialList course (courseMaterials course)
    makeItem "" >>= layout "templates/block-course.html" ctx siteCtx

renderClosedCourse :: Context String -> Course -> Compiler (Item String)
renderClosedCourse siteCtx course = makeItem ""
    >>= layout "templates/placeholder.html" ctx (constField "noindex" "true" <> siteCtx)
  where
    ctx = titleAndDescription (courseTitle course) (courseDescription course)
        <> constField "category" "Lecture"

renderSession :: Context String -> Site -> Course -> SectionId -> Session -> Item String -> Compiler (Item String)
renderSession siteCtx site course key session = layout "templates/block-session.html" ctx siteCtx
  where
    ctx = titleAndDescription (sessionTitle session) ""
        <> courseLink course
        <> boolField "numbered" (const (isNumbered key))
        <> materialList course (sessionMaterials session)
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

    isNumbered (Numbered _) = True
    isNumbered (Supplement _) = False

renderMaterial :: Context String -> Course -> Material -> Item String -> Compiler (Item String)
renderMaterial siteCtx course material body
    | courseOpen course = wrap (constField "post" "true") body
    | otherwise = loadAndApplyTemplate "templates/placeholder.html"
        (constField "category" "Lecture" <> ctx) (fmap (const "") body)
        >>= wrap (constField "noindex" "true")
  where
    ctx = titleAndDescription (materialTitle material) ""
        <> courseLink course
        <> boolField "open" (const (courseOpen course))
        <> rows "anchors" (value "anchor" id) (const (Set.toList (materialAnchors material)))
    wrap flags = layout "templates/block-material.html" ctx (flags <> siteCtx)

courseLink :: Course -> Context a
courseLink course = constField "courseTitle" (escapeHtml (courseTitle course))
    <> constField "courseUrl" (siteUrl (coursePath course))

materialList :: Course -> [String] -> Context a
materialList course names = boolField "hasMaterials" (const (not (null names)))
    <> rows "materials" (linkContext materialTitle (materialPath course))
        (const [coursePages course Map.! name | name <- names])

renderPage :: Context String -> Site -> Block -> Page -> Item String -> Compiler (Item String)
renderPage siteCtx site block page body = do
    fieldData <- maybe (fail ("Missing field for block: " ++ blockName block)) pure
        (find ((== blockField block) . fieldName) (siteFields site))
    let blocks = map (blockNamed site) (fieldBlocks fieldData)
        currentPage = find ((== pageKey page) . pageKey . middle) (adjacent (blockPages block))
        anchors = [anchor | (anchor, key) <- Map.toList (blockAnchors block), key == pageKey page]
        occurrences = courseOccurrences site block
        prerequisites = map (blockNamed site) (blockRequires block)
        closedPageNavigation = boolField "hasPageNavigation" (const (length (blockPages block) > 1))
            <> neighborContext navigationPageTitle (pagePath block) (const False)
                (const (currentPage >>= previous)) (const (currentPage >>= next))
        ctx = titleAndDescription (pageTitle page) ""
            <> fieldIndex fieldData blocks (Just (block, page))
            <> boolField "open" (const (blockOpen block))
            <> boolField "introPage" (const (pageKey page == "index"))
            <> boolField "hasPrerequisites" (const (not (null prerequisites)))
            <> rows "prerequisites" blockLinkContext (const prerequisites)
            <> boolField "hasOccurrences" (const (not (null occurrences)))
            <> rows "occurrences" (occurrenceContext site) (const occurrences)
            <> rows "anchors" (value "anchor" id) (const anchors)
            <> closedPageNavigation
        pageTemplate = "templates/block-page.html"
        flags
            | blockOpen block = mempty
            | otherwise       = constField "noindex" "true"
    narrowNavigation <- loadAndApplyTemplate "templates/block-field-nav.html" ctx body
    cards <- if blockOpen block
        then itemBody <$> loadAndApplyTemplate "templates/block-page-cards.html"
            (fieldPageNavigation fieldData blocks block page) body
        else pure ""

    pageBody <- if blockOpen block
        then pure body
        else loadAndApplyTemplate "templates/placeholder.html"
            (constField "category" "Lecture" <> ctx) (fmap (const "") body)
    let pageContext = constField "pageNavigationCards" cards <> ctx
        afterTitle
            | pageKey page == "index" = narrowNavigation
            | otherwise = fmap (++ cards) narrowNavigation
    loadAndApplyTemplate pageTemplate (pageContext <> bodyField "body") pageBody
        >>= insertAfterTitle afterTitle
        >>= layoutContent ctx (flags <> siteCtx)

fieldPageNavigation :: Field -> [Block] -> Block -> Page -> Context a
fieldPageNavigation fieldData blocks block page =
    neighborContext fst snd (const False) (const (target previous)) (const (target next))
  where
    publicPages = [(b, p) | b <- blocks, blockOpen b, p <- blockPages b]
    here = pagePath block page
    current = find ((== here) . uncurry pagePath . middle) (adjacent publicPages)
    target select = Just $ maybe (fieldTitle fieldData ++ "の目次", fieldPath fieldData) destination
        (current >>= select)
    destination (b, p) = (label, pagePath b p)
      where
        label
            | blockName b == blockName block || pageTitle p == blockTitle b = pageTitle p
            | otherwise = pageTitle p ++ " (" ++ blockTitle b ++ ")"

-- | Insert navigation after the first h1 without re-rendering its inline HTML
-- or the rest of the page. The title comes from the template or Pandoc.
insertAfterTitle :: Item String -> Item String -> Compiler (Item String)
insertAfterTitle navigation page
    | T.null fromTitleEnd = fail ("Missing h1 on " ++ toFilePath (itemIdentifier page))
    | otherwise = pure (fmap (const updated) page)
  where
    closingTag = "</h1>"
    (beforeTitleEnd, fromTitleEnd) = T.breakOn closingTag (T.pack (itemBody page))
    updated = T.unpack $ beforeTitleEnd <> closingTag <> "\n" <> T.pack (itemBody navigation)
        <> T.drop (T.length closingTag) fromTitleEnd

type Position = (SectionId, Session, Entry)
type Neighbors a = (Maybe a, a, Maybe a)

courseOccurrences :: Site -> Block -> [(Course, Neighbors Position)]
courseOccurrences site block =
    [ (course, place)
    | course <- siteCourses site
    , courseOpen course
    , place <- adjacent [(key, session, entry) | (key, session) <- courseSections course, entry <- sessionBlocks session]
    , entryBlock (positionEntry (middle place)) == blockName block
    ]

occurrenceContext :: Site -> Context (Course, Neighbors Position)
occurrenceContext site =
    value "courseTitle" (courseTitle . fst)
    <> field "courseUrl" (pure . siteUrl . coursePath . fst . itemBody)
    <> value "sessionLabel" (positionLabel . middle . snd)
    <> field "sessionUrl" (\item -> let (course, place) = itemBody item
                                        (key, _, _) = middle place
                                    in pure (siteUrl (sectionPath course key)))
    <> value "level" (levelLabel . entryLevel . positionEntry . middle . snd)
    <> neighborContext (blockTitle . positionBlock) (blockPath . positionBlock)
        (not . blockOpen . positionBlock) (previous . snd) (next . snd)
  where
    positionBlock = blockNamed site . entryBlock . positionEntry
    positionLabel (Numbered n, _, _) = "第 " ++ show n ++ " 回"
    positionLabel (Supplement _, session, _) = sessionTitle session

positionEntry :: Position -> Entry
positionEntry (_, _, entry) = entry

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
    | pageKey page == "index" =
        let P.Pandoc _ blocks = pageDoc page
        in case [T.unpack (stringify title) | P.Header _ _ title <- blocks] of
            first : _ -> first
            [] -> "導入"
    | otherwise = pageTitle page

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
    >>= layoutContent ctx siteCtx

-- | Apply the shared wrappers to the output of a page template.
layoutContent :: Context String -> Context String -> Item String -> Compiler (Item String)
layoutContent ctx siteCtx item =
    loadAndApplyTemplate "templates/content.html" (bodyField "body") item
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
