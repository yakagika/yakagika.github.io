{-# LANGUAGE OverloadedStrings #-}
module BlockModel where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Aeson (FromJSON(..), Value(Object), withObject, (.:), (.:!), (.:?), (.!=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower)
import Data.Generics (everywhere, everything, mkQ, mkT)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (find, foldl', intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Hakyll (toUrl)
import Hakyll.Core.Provider.Metadata (parsePage)
import Network.URI (escapeURIString, isUnreserved, unEscapeString)
import System.FilePath
import qualified Text.HTML.TagSoup as HTML
import qualified Text.Pandoc as P
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query, walk, walkM)

data Site = Site { siteFields :: [Field], siteBlocks :: M.Map String Block, siteCourses :: [Course] }
data Field = Field { fieldName, fieldTitle, fieldDescription :: String, fieldSource :: FilePath,
                     fieldBlocks :: [String] }
data Block = Block { blockName, blockTitle, blockField, blockKind :: String, blockSource :: FilePath,
                     blockOpen :: Bool, blockRequires :: [String], blockBibliography :: Maybe String,
                     blockDocument :: P.Pandoc, blockPages :: [Page],
                     blockMergedSection :: Maybe String,
                     blockAnchors, blockExercises :: M.Map String String }
data Page = Page { pageKey, pageTitle :: String, pageDoc :: P.Pandoc }
data Course = Course { courseName, courseTitle, courseDescription :: String, courseSource :: FilePath,
                       courseOpen :: Bool, courseSessions :: [Session],
                       courseMaterials :: [String], courseSupplements :: [(String, Session)],
                       coursePages :: M.Map String Material }
data Session = Session { sessionTitle :: String, sessionIntro :: String, sessionBlocks :: [Entry],
                         sessionMaterials :: [String], sessionPages :: Maybe [[String]] }
data SectionId = Numbered Int | Supplement String
    deriving Eq
data Material = Material { materialName, materialTitle :: String, materialSource :: FilePath,
                           materialBibliography :: Maybe String, materialDoc :: P.Pandoc,
                           materialAnchors :: S.Set String }
data Entry = Entry { entryBlock, entryLevel :: String, entryExercises :: [Exercise] }
data Exercise = Exercise { exerciseId :: String, exerciseFile :: Maybe String }
data Lesson = Lesson { lessonNumber :: Int, lessonBlocks :: [LessonBlock] }
data LessonBlock = LessonBlock { lessonEntry :: Entry, lessonBlock :: Block,
                                 lessonRenamedIds :: M.Map String String }
data CourseSection = CourseSection
    { sectionId :: SectionId, sectionSession :: Session, sectionLessons :: [Lesson],
      sectionDestination :: FilePath }

fieldPath :: Field -> FilePath
fieldPath f = "fields" </> fieldName f </> "index.html"

pagePath :: Block -> Page -> FilePath
pagePath b p = blockDir b </> pageKey p <.> "html"

blockDir :: Block -> FilePath
blockDir b = "fields" </> blockField b </> blockName b

blockPath :: Block -> FilePath
blockPath b = blockDir b </> "index.html"

coursePath :: Course -> FilePath
coursePath c = "courses" </> courseName c </> "index.html"

coursePagePath :: Course -> String -> FilePath
coursePagePath c key = takeDirectory (coursePath c) </> key <.> "html"

sectionPath :: Course -> String -> FilePath
sectionPath = coursePagePath

courseSections :: Course -> [(SectionId, Session)]
courseSections c = zipWith (\n s -> (Numbered n, s)) [1..] (courseSessions c)
    ++ [(Supplement key, s) | (key,s) <- courseSupplements c]

materialPath :: Course -> Material -> FilePath
materialPath c = coursePagePath c . materialName

lessonPath :: Course -> Lesson -> FilePath
lessonPath c lesson = takeDirectory (coursePath c) </> show (lessonNumber lesson)
    </> blockName (lessonBlock (head (lessonBlocks lesson))) <.> "html"

lessonTitle :: Lesson -> String
lessonTitle = intercalate " / " . map (blockTitle . lessonBlock) . lessonBlocks

lessonOpen :: Lesson -> Bool
lessonOpen = any (blockOpen . lessonBlock) . lessonBlocks

sectionTitle :: SectionId -> Session -> String
sectionTitle (Numbered n) s = "第 " ++ show n ++ " 回 " ++ sessionTitle s
sectionTitle (Supplement _) s = sessionTitle s

instance FromJSON Session where
  parseJSON = withObject "session" $ \o ->
    Session <$> o .: "title" <*> o .:? "intro" .!= "" <*> o .: "blocks" <*> o .:? "materials" .!= []
        <*> o .:! "pages"
instance FromJSON Entry where
  parseJSON = withObject "course block" $ \o -> Entry <$> o .: "block" <*> o .: "level" <*> o .:? "exercises" .!= []
instance FromJSON Exercise where
  parseJSON = withObject "exercise" $ \o -> Exercise <$> o .: "id" <*> o .:? "file"

-- Keep the physical source (including the fixture prefix) separate from its public name.
readYaml :: FilePath -> (Value -> Y.Parser a) -> IO a
readYaml path parser = do
  value <- Y.decodeFileEither path >>= either (failAt path . Y.prettyPrintParseException) pure
  either (failAt path) pure (parseEither parser value)

failAt :: MonadFail m => FilePath -> String -> m a
failAt path msg = fail (path ++ ": " ++ msg)

readField :: FilePath -> IO Field
readField path = readYaml path $ withObject "field" $ \o ->
  Field (takeFileName $ takeDirectory path) <$> o .: "title" <*> o .: "description" <*> pure path <*> o .: "blocks"

readCourse :: FilePath -> IO Course
readCourse path = readYaml path $ withObject "course" $ \o ->
  Course (takeBaseName path) <$> o .: "title" <*> o .: "description" <*> pure path
    <*> o .:! "open" .!= False <*> o .: "sessions"
    <*> o .:? "materials" .!= [] <*> (o .:? "supplements" .!= [] >>= mapM supplement)
    <*> pure M.empty
  where
    supplement = withObject "supplement" $ \o -> (,) <$> o .: "id" <*> parseJSON (Object o)

markdown :: P.ReaderOptions -> String -> Either String P.Pandoc
markdown opts = either (Left . show) Right . P.runPure . P.readMarkdown opts . T.pack

readMaterial :: P.ReaderOptions -> FilePath -> IO Material
readMaterial opts path = do
  source <- readFile path
  (metadata, body) <- either (failAt path . show) pure (parsePage source)
  (title, bibliography) <- either (failAt path) pure $
    flip parseEither (Object metadata) $ withObject "course material" $ \o -> do
      when (KM.member "open" o) $ fail "material inherits open from its course"
      (,) <$> o .: "title" <*> o .:? "bibliography"
  doc <- either (failAt path) pure $ markdown opts body
  let ids = documentIds doc
  checkBibliographyId path doc
  unique path "ID" [(i,path) | i <- ids]
  pure $ Material (takeBaseName path) title path bibliography doc (S.fromList ids)

readBlock :: P.ReaderOptions -> FilePath -> IO Block
readBlock opts path = do
  source <- readFile path
  (metadata, body) <- either (failAt path . show) pure (parsePage source)
  (title, kind, open, requires, bibliography) <- either (failAt path) pure $
    flip parseEither (Object metadata) $ withObject "block" $ \o ->
      (,,,,) <$> o .: "title" <*> o .: "kind" <*> o .:? "open" .!= False
             <*> o .:? "requires" .!= [] <*> o .:? "bibliography"
  unless (kind `elem` ["theory","code"]) $ failAt path ("unknown kind " ++ kind)
  let readWith options = either (failAt path) pure $ markdown options body
  -- A normally parsed Header cannot tell an explicit ID from Pandoc's generated ID.
  -- The second read inspects only explicit attributes; the published AST retains auto IDs.
  P.Pandoc _ explicit <- readWith opts {P.readerExtensions =
      P.disableExtension P.Ext_auto_identifiers (P.readerExtensions opts)}
  forM_ [ (i, stringify t) | P.Header 1 (i,_,_) t <- explicit ] $ \(i,t) ->
    unless (not (T.null i) && T.all (\c -> isAsciiLower c || c `elem` ['0'..'9'] || c == '-') i) $
      failAt path ("expected explicit [a-z0-9-]+ ID on heading " ++ T.unpack t ++ " (" ++ T.unpack i ++ ")")
  let exerciseHeading (P.Header n (i,_,_) t)
        | "Exercise " `T.isPrefixOf` stringify t || "exercise-" `T.isPrefixOf` i =
            [(n,T.unpack i,T.unpack (stringify t))]
      exerciseHeading _ = []
  forM_ (query exerciseHeading explicit) $ \(n,i,t) ->
    unless (n == 2 && "Exercise " `isPrefixOf` t && "exercise-" `isPrefixOf` i) $
      failAt path ("expected ## Exercise heading with explicit exercise- ID: " ++ t)
  P.Pandoc meta blocks <- readWith opts
  let (pages, mergedSection) = splitPages meta title blocks
      ids = [(i, pageKey p) | p <- pages, i <- documentIds (pageDoc p)]
  checkBibliographyId path (P.Pandoc meta blocks)
  unique path "ID" ids
  exercises <- fmap concat $ forM pages $ \p -> do
    forM (query exerciseHeading (pageDoc p)) $ \(_,i,t) -> do
      unless (pageKey p == "exercises") $
        failAt path ("exercise heading must be on exercises page: " ++ t)
      pure (i,t)
  pure $ Block (takeBaseName path) title (takeFileName $ takeDirectory path) kind path
               open requires bibliography (P.Pandoc meta blocks) pages mergedSection
               (M.fromList ids) (M.fromList exercises)

checkBibliographyId :: FilePath -> P.Pandoc -> IO ()
checkBibliographyId path doc =
  when (or $ query refsDiv doc) $ failAt path "reserved bibliography ID #refs on Div"
  where
    refsDiv (P.Div (i,_,_) _) = [i == "refs"]
    refsDiv _ = []

splitPages :: P.Meta -> String -> [P.Block] -> ([Page], Maybe String)
splitPages meta title blocks =
  let (here,rest) = break isBoundary blocks
      isBoundary (P.Header 1 _ _) = True
      isBoundary _ = False
      -- Include the boundary heading in its page, so links keep their fragment.
      next (h@(P.Header 1 (i,_,_) t):bs) =
        let (section,remaining) = break isBoundary bs
        in Page (T.unpack i) (T.unpack $ stringify t) (P.Pandoc meta (h:section)) : next remaining
      next _ = []
      index body = Page "index" title (P.Pandoc meta body)
      hasHeading (P.Header _ _ _) = [True]
      hasHeading _ = []
  in case next rest of
      Page key _ (P.Pandoc _ section) : remaining
        | key /= "exercises", not (or (query hasHeading here)) ->
            (index (here ++ section) : remaining, Just key)
      pages -> (index here : pages, Nothing)

documentIds :: P.Pandoc -> [String]
documentIds (P.Pandoc _ blocks) =
  everything (++) ([] `mkQ` attrId) blocks ++ query rawBlock blocks ++ query rawInline blocks
  where
    attrId :: P.Attr -> [String]
    attrId (i,_,_) = [T.unpack i | not (T.null i)]
    rawBlock (P.RawBlock (P.Format "html") html) = htmlIds html
    rawBlock _ = []
    rawInline (P.RawInline (P.Format "html") html) = htmlIds html
    rawInline _ = []

htmlIds :: T.Text -> [String]
htmlIds html = [T.unpack i | HTML.TagOpen _ attrs <- HTML.parseTags html,
                             (name,i) <- attrs, T.toLower name == "id", not (T.null i)]

planCourseSections :: Site -> Course -> IO [CourseSection]
planCourseSections site course = forM (courseSections course) $ \(key, session) -> do
    let err = failAt (courseSource course) . ((sectionTitle key session ++ ": pages: ") ++)
        required = filter ((== "required") . entryLevel) (sessionBlocks session)
        entries = M.fromList [(entryBlock entry, entry) | entry <- required]
        names = map entryBlock required
    lessons <- case key of
      Supplement _ -> do
        forM_ (sessionPages session) $ \_ -> err "only numbered sessions can specify pages"
        pure []
      Numbered number -> do
        groups <- case sessionPages session of
          Nothing -> pure (map (:[]) required)
          Just pages -> do
            when (any null pages) $ err "empty page"
            unless (concat pages == names && S.size (S.fromList names) == length names) $
              err "list each required block once, in blocks order"
            pure (map (map (entries M.!)) pages)
        pure [Lesson number (groupBlocks group) | group <- groups]
    destination <- sectionTarget course key session lessons
    pure (CourseSection key session lessons destination)
  where
    groupBlocks = reverse . snd . foldl' addBlock (S.empty, [])
    addBlock (seen, soFar) entry =
      let block = siteBlocks site M.! entryBlock entry
          ids = M.keysSet (blockAnchors block)
          renamed = M.fromSet (\i -> blockName block ++ "-" ++ i) (S.intersection seen ids)
      in (S.union seen ids, LessonBlock entry block renamed : soFar)

sectionTarget :: Course -> SectionId -> Session -> [Lesson] -> IO FilePath
sectionTarget course key session lessons = case key of
    Supplement slug -> pure (sectionPath course slug)
    Numbered _ -> case lessons of
      first : _ -> pure (lessonPath course first)
      [] -> case sessionMaterials session of
        [] -> err "numbered session needs at least one lesson or material"
        first : _ -> do
          unless (null (sessionIntro session)) $ err "session intro requires a lesson"
          pure (materialPath course (coursePages course M.! first))
  where
    err message = failAt (courseSource course) (sectionTitle key session ++ ": " ++ message)

lessonAnchor :: LessonBlock -> String -> String
lessonAnchor b anchor = M.findWithDefault anchor anchor (lessonRenamedIds b)

anchorSuffix :: String -> String
anchorSuffix anchor = '#' : escapeURIString isUnreserved anchor

lessonDocument :: Site -> Course -> [Lesson] -> Lesson -> LessonBlock -> Either String P.Pandoc
lessonDocument site course lessons lesson part = do
    linked <- resolveLinksWith site Nothing (Just course) (Just (resolveLessonTarget course lessons key current))
        (blockDocument block)
    pure (renameDocumentIds (lessonRenamedIds part) $ walk addFile linked)
  where
    block = lessonBlock part
    key = Numbered (lessonNumber lesson)
    current = Just (lesson, part)
    files = M.fromList [(exerciseId ex, name) | ex <- entryExercises (lessonEntry part),
                                                 Just name <- [exerciseFile ex]]
    addFile = concatMap withFile
    withFile heading@(P.Header _ (anchor, _, _) _)
      | Just name <- M.lookup (T.unpack anchor) files =
        [heading, P.RawBlock (P.Format "html")
          (T.concat ["<p class=\"exercise-file\">提出ファイル <code>", HTML.escapeHTML (T.pack name), "</code></p>"])]
    withFile blockItem = [blockItem]

resolveLessonTarget :: Course -> [Lesson] -> SectionId -> Maybe (Lesson, LessonBlock)
                    -> String -> Maybe (Either String String)
resolveLessonTarget course lessons key current url
    | Just ref <- stripPrefix "block:" url =
        let (name, fragment) = break (== '#') ref
            matching = [(lesson, part) | lesson <- lessons, part <- lessonBlocks lesson,
                                          blockName (lessonBlock part) == unEscapeString name]
            sameRound (lesson, _) = Numbered (lessonNumber lesson) == key
        in fmap (\target -> destination target (stripPrefix "#" fragment))
            (find sameRound matching <|> listToMaybe matching)
    | Just fragment <- stripPrefix "#" url, Just target <- current = Just (destination target (Just fragment))
    | otherwise = Nothing
  where
    destination (lesson, part) fragment = do
      let path = toUrl (lessonPath course lesson)
          block = lessonBlock part
      case fragment of
        Nothing -> Right path
        Just encoded -> do
          let anchor = unEscapeString encoded
              local = maybe False ((== lessonPath course lesson) . lessonPath course . fst) current
          unless (M.member anchor (blockAnchors block)) $
            Left ("unknown ID: block:" ++ blockName block ++ "#" ++ encoded)
          Right $ (if local then "" else path) ++ anchorSuffix (lessonAnchor part anchor)

renameDocumentIds :: M.Map String String -> P.Pandoc -> P.Pandoc
renameDocumentIds names
    | M.null names = id
    | otherwise = walk renameRawInline . walk renameRawBlock . everywhere (mkT renameAttr)
  where
    replace old = maybe old T.pack (M.lookup (T.unpack old) names)
    renameAttr :: P.Attr -> P.Attr
    renameAttr (anchor, classes, attrs) = (replace anchor, classes, attrs)
    renameRawBlock (P.RawBlock format@(P.Format "html") html) = P.RawBlock format (renameHtml html)
    renameRawBlock b = b
    renameRawInline (P.RawInline format@(P.Format "html") html) = P.RawInline format (renameHtml html)
    renameRawInline i = i
    renameHtml html
      | any (`M.member` names) (htmlIds html) = HTML.renderTags (map renameTag (HTML.parseTags html))
      | otherwise = html
    renameTag (HTML.TagOpen tag attrs) = HTML.TagOpen tag (map renameId attrs)
    renameTag tag = tag
    renameId (name, anchor)
      | T.toLower name == "id" = (name, replace anchor)
    renameId attr = attr

unique :: MonadFail m => FilePath -> String -> [(String,String)] -> m ()
unique source label = go M.empty
  where
    go _ [] = pure ()
    go seen ((k,origin):xs) = case M.lookup k seen of
      Just other -> failAt source ("duplicate " ++ label ++ " " ++ k ++ " (" ++ other ++ ", " ++ origin ++ ")")
      Nothing -> go (M.insert k origin seen) xs

readSite :: P.ReaderOptions -> [FilePath] -> [FilePath] -> [FilePath] -> [FilePath] -> IO Site
readSite opts fieldFiles blockFiles courseFiles materialFiles = do
  fields <- mapM readField fieldFiles
  blocks <- mapM (readBlock opts) blockFiles
  courseInputs <- mapM readCourse courseFiles
  materials <- mapM (readMaterial opts) materialFiles
  unique "fields" "block" [(blockName b, blockSource b) | b <- blocks]
  unique "fields" "field" [(fieldName f, fieldSource f) | f <- fields]
  unique "courses" "course" [(courseName c, courseSource c) | c <- courseInputs]
  let parentCourse = takeFileName . takeDirectory . materialSource
      materialsByCourse = M.fromListWith (++) [(parentCourse m,[m]) | m <- materials]
  forM_ materials $ \m ->
    unless (any ((== parentCourse m) . courseName) courseInputs) $
      failAt (materialSource m) ("unknown course " ++ parentCourse m)
  courses <- forM courseInputs $ \c ->
    attachMaterials c (M.findWithDefault [] (courseName c) materialsByCourse)
  let catalog = M.fromList [(blockName b,b) | b <- blocks]
      site = Site fields catalog courses
  forM_ blocks $ \b -> do
    unless (any (\f -> fieldName f == blockField b && blockName b `elem` fieldBlocks f) fields) $
      failAt (blockSource b) "block is missing from field.yaml"
    forM_ (blockRequires b) $ \r -> unless (M.member r catalog) $
      failAt (blockSource b) ("unknown requires: " ++ r)
  forM_ (stronglyConnComp [(b, blockName b, blockRequires b) | b <- blocks]) $ \scc -> case scc of
    CyclicSCC bs -> fail $ "requires cycle: " ++ unwords (map blockSource bs)
    AcyclicSCC _ -> pure ()
  forM_ fields $ \f -> do
    unique (fieldSource f) "block in field order" [(n,n) | n <- fieldBlocks f]
    let check seen n = case M.lookup n catalog of
          Just b | blockField b == fieldName f -> do
            forM_ (blockRequires b) $ \r ->
              when (blockField (catalog M.! r) == fieldName f && S.notMember r seen) $
                failAt (fieldSource f) (n ++ " requires earlier block " ++ r)
            pure $ S.insert n seen
          _ -> failAt (fieldSource f) ("missing block file: " ++ n)
    _ <- foldM check S.empty (fieldBlocks f)
    pure ()
  forM_ courses $ \c -> do
    let check seen e = do
          let name = entryBlock e
              err = failAt (courseSource c)
          b <- maybe (err ("unknown block " ++ name)) pure (M.lookup name catalog)
          unless (entryLevel e `elem` ["required","advanced"]) $ err ("unknown level " ++ entryLevel e)
          forM_ (entryExercises e) $ \ex -> unless (M.member (exerciseId ex) $ blockExercises b) $
            err ("unknown exercise " ++ name ++ "#" ++ exerciseId ex)
          if entryLevel e == "advanced" then pure seen else do
            forM_ (blockRequires b) $ \r -> unless (S.member r seen) $
              err ("required block " ++ name ++ " requires earlier required block " ++ r)
            pure $ S.insert name seen
    _ <- foldM check S.empty (concatMap (sessionBlocks . snd) $ courseSections c)
    pure ()
  -- Resolve during registration, including closed bodies, so a broken reference fails
  -- even when its target page would otherwise come from the cache.
  resolved <- forM blocks $ \b -> do
    pages <- forM (blockPages b) $ \p -> do
      doc <- either (failAt $ blockSource b) pure $
        resolveLinks site (Just (b,p)) Nothing (pageDoc p)
      pure p {pageDoc = doc}
    pure (blockName b,b {blockPages = pages})
  resolvedCourses <- forM courses $ \c -> do
    pages <- forM (coursePages c) $ \m -> do
      doc <- either (failAt $ materialSource m) pure $
        resolveLinks site Nothing (Just c) (materialDoc m)
      pure m {materialDoc = doc}
    pure c {coursePages = pages}
  pure site {siteBlocks = M.fromList resolved, siteCourses = resolvedCourses}

attachMaterials :: Course -> [Material] -> IO Course
attachMaterials c materials = do
  let source = courseSource c
      pages = M.fromList [(materialName m,m) | m <- materials]
      sections = courseSections c
      materialLists = (coursePath c, courseMaterials c)
        : [(sectionTitle key s, sessionMaterials s) | (key,s) <- sections, Numbered _ <- [key]]
        ++ [(sectionPath c key, sessionMaterials s) | (key,s) <- courseSupplements c]
      referenced = S.fromList (concatMap snd materialLists)
      supplements = map fst (courseSupplements c)
      routes = [(coursePath c,source)]
        ++ [(sectionPath c key,source) | key <- supplements]
        ++ [(materialPath c m,materialSource m) | m <- materials]
  unique source "output path" routes
  forM_ materialLists $ \(path,names) ->
    unique source ("material in " ++ path) [(name,name) | name <- names]
  forM_ supplements $ \key -> unless (validSlug key) $
    failAt source ("invalid supplement id: " ++ key)
  forM_ materials $ \m -> do
    unless (validSlug (materialName m)) $
      failAt (materialSource m) ("invalid material name: " ++ materialName m)
    unless (S.member (materialName m) referenced) $
      failAt (materialSource m) ("material is missing from " ++ source)
  forM_ referenced $ \name -> unless (M.member name pages) $
    failAt source ("missing material file: " ++ name)
  pure c {coursePages = pages}
  where
    validSlug (c0:cs) = isAsciiLower c0 && all (\x -> isAsciiLower x || x `elem` ['0'..'9'] || x == '-') cs
    validSlug [] = False

resolveLinks :: Site -> Maybe (Block,Page) -> Maybe Course -> P.Pandoc -> Either String P.Pandoc
resolveLinks site current course = resolveLinksWith site current course Nothing

resolveLinksWith :: Site -> Maybe (Block, Page) -> Maybe Course
                 -> Maybe (String -> Maybe (Either String String)) -> P.Pandoc -> Either String P.Pandoc
resolveLinksWith site current course override doc = do
    linked <- walkM rewrite doc
    case override of
      Nothing -> pure linked
      Just _ -> walkM rewriteRawBlock linked >>= walkM rewriteRawInline
  where
    rewrite (P.Link a label (url,title)) = do
      target <- resolve (T.unpack url)
      pure $ P.Link a label (T.pack target,title)
    rewrite x = Right x
    rewriteRawBlock (P.RawBlock format@(P.Format "html") html) = P.RawBlock format <$> rewriteHtml html
    rewriteRawBlock b = pure b
    rewriteRawInline (P.RawInline format@(P.Format "html") html) = P.RawInline format <$> rewriteHtml html
    rewriteRawInline i = pure i
    rewriteHtml html = do
      let tags = HTML.parseTags html
      updated <- mapM rewriteTag tags
      pure (if tags == updated then html else HTML.renderTags updated)
    rewriteTag (HTML.TagOpen tag attrs)
      | T.toLower tag == "a" = HTML.TagOpen tag <$> mapM rewriteHref attrs
    rewriteTag tag = pure tag
    rewriteHref (name, url)
      | T.toLower name == "href" && url /= "#" = (,) name . T.pack <$> resolve (T.unpack url)
    rewriteHref attr = pure attr
    resolve url = fromMaybe (defaultTarget url) (override >>= ($ url))
    defaultTarget url
      | Just ref <- stripPrefix "block:" url = do
          let (name,fragment) = break (== '#') ref
          b <- maybe (Left $ "unknown block link: " ++ url) Right $
                 M.lookup (unEscapeString name) (siteBlocks site)
          if null fragment then Right (toUrl $ blockPath b) else anchor b (tail fragment)
      | Just ref <- stripPrefix "material:" url = do
          c <- maybe (Left $ "material link outside its course: " ++ url) Right course
          let (name,fragment) = break (== '#') ref
          m <- maybe (Left $ "unknown material link: " ++ url) Right $
                 M.lookup (unEscapeString name) (coursePages c)
          unless (null fragment || S.member (unEscapeString $ tail fragment) (materialAnchors m)) $
            Left ("unknown ID: " ++ url)
          Right (toUrl (materialPath c m) ++ fragment)
      | Just fragment <- stripPrefix "#" url, Just (b,_) <- current = anchor b fragment
      | Just fragment <- stripPrefix "#" url, Just _ <- course =
          if S.member (unEscapeString fragment) localIds
            then Right url else Left ("unknown local ID: " ++ url)
      | otherwise = Right url
    localIds = S.fromList (documentIds doc)
    anchor b fragment = do
      let idText = unEscapeString fragment
      key <- maybe (Left $ "unknown ID: block:" ++ blockName b ++ "#" ++ fragment) Right $
               M.lookup idText (blockAnchors b)
      let samePage = maybe False (\(here,p) -> blockName here == blockName b && pageKey p == key) current
          path = if samePage then "" else toUrl (blockDir b </> key <.> "html")
      Right (path ++ "#" ++ fragment)

sessionDoc :: P.ReaderOptions -> Site -> Course -> [Lesson] -> CourseSection -> S.Set String
           -> Either String P.Pandoc
sessionDoc opts site c lessons section lessonIds = markdown opts (sessionIntro (sectionSession section))
    >>= resolveLinksWith site Nothing (Just c) (Just target)
  where
    target url
      | Just fragment <- stripPrefix "#" url
      , S.member (unEscapeString fragment) lessonIds = Just (Right url)
      | otherwise = resolveLessonTarget c lessons (sectionId section) Nothing url
