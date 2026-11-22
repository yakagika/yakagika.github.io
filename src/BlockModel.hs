{-# LANGUAGE OverloadedStrings #-}
module BlockModel where

import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Aeson (FromJSON(..), Value(Object), withObject, (.:), (.:!), (.:?), (.!=))
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower)
import Data.Generics (everything, mkQ)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Hakyll (toUrl)
import Hakyll.Core.Provider.Metadata (parsePage)
import Network.URI (unEscapeString)
import System.FilePath
import qualified Text.HTML.TagSoup as HTML
import qualified Text.Pandoc as P
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query, walkM)

data Site = Site { siteFields :: [Field], siteBlocks :: M.Map String Block, siteCourses :: [Course] }
data Field = Field { fieldName, fieldTitle, fieldDescription :: String, fieldSource :: FilePath,
                     fieldBlocks :: [String] }
data Block = Block { blockName, blockTitle, blockField, blockKind :: String, blockSource :: FilePath,
                     blockOpen :: Bool, blockRequires :: [String], blockBibliography :: Maybe String,
                     blockPages :: [Page], blockAnchors, blockExercises :: M.Map String String }
data Page = Page { pageKey, pageTitle :: String, pageDoc :: P.Pandoc }
data Course = Course { courseName, courseTitle, courseDescription :: String, courseSource :: FilePath,
                       courseOpen :: Bool, courseSessions :: [Session] }
data Session = Session { sessionTitle :: String, sessionIntro :: String, sessionBlocks :: [Entry] }
data Entry = Entry { entryBlock, entryLevel :: String, entryExercises :: [Exercise] }
data Exercise = Exercise { exerciseId :: String, exerciseFile :: Maybe String }

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

sessionPath :: Course -> Int -> FilePath
sessionPath c n = "courses" </> courseName c </> show n <.> "html"

instance FromJSON Session where
  parseJSON = withObject "session" $ \o -> Session <$> o .: "title" <*> o .:? "intro" .!= "" <*> o .: "blocks"
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

markdown :: P.ReaderOptions -> String -> Either String P.Pandoc
markdown opts = either (Left . show) Right . P.runPure . P.readMarkdown opts . T.pack

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
  let pages = splitPages meta title blocks
      ids = [(i, pageKey p) | p <- pages, i <- documentIds (pageDoc p)]
      refsDiv (P.Div (i,_,_) _) = [i == "refs"]
      refsDiv _ = []
  when (or $ query refsDiv blocks) $ failAt path "reserved bibliography ID #refs on Div"
  unique path "ID" ids
  exercises <- fmap concat $ forM pages $ \p -> do
    forM (query exerciseHeading (pageDoc p)) $ \(_,i,t) -> do
      unless (pageKey p == "exercises") $
        failAt path ("exercise heading must be on exercises page: " ++ t)
      pure (i,t)
  pure $ Block (takeBaseName path) title (takeFileName $ takeDirectory path) kind path
               open requires bibliography pages (M.fromList ids) (M.fromList exercises)

splitPages :: P.Meta -> String -> [P.Block] -> [Page]
splitPages meta title blocks =
  let (here,rest) = break isBoundary blocks
      isBoundary (P.Header 1 _ _) = True
      isBoundary _ = False
      -- Include the boundary heading in its page, so links keep their fragment.
      next (h@(P.Header 1 (i,_,_) t):bs) =
        let (section,remaining) = break isBoundary bs
        in Page (T.unpack i) (T.unpack $ stringify t) (P.Pandoc meta (h:section)) : next remaining
      next _ = []
  in Page "index" title (P.Pandoc meta here) : next rest

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
    htmlIds html = [T.unpack i | HTML.TagOpen _ attrs <- HTML.parseTags html,
                                 (name,i) <- attrs, T.toLower name == "id", not (T.null i)]

unique :: MonadFail m => FilePath -> String -> [(String,String)] -> m ()
unique source label = go M.empty
  where
    go _ [] = pure ()
    go seen ((k,origin):xs) = case M.lookup k seen of
      Just other -> failAt source ("duplicate " ++ label ++ " " ++ k ++ " (" ++ other ++ ", " ++ origin ++ ")")
      Nothing -> go (M.insert k origin seen) xs

readSite :: P.ReaderOptions -> [FilePath] -> [FilePath] -> [FilePath] -> IO Site
readSite opts fieldFiles blockFiles courseFiles = do
  fields <- mapM readField fieldFiles
  blocks <- mapM (readBlock opts) blockFiles
  courses <- mapM readCourse courseFiles
  unique "fields" "block" [(blockName b, blockSource b) | b <- blocks]
  unique "fields" "field" [(fieldName f, fieldSource f) | f <- fields]
  unique "courses" "course" [(courseName c, courseSource c) | c <- courses]
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
    _ <- foldM check S.empty (concatMap sessionBlocks $ courseSessions c)
    pure ()
  -- Resolve during registration, including closed bodies, so a broken reference fails
  -- even when its target page would otherwise come from the cache.
  resolved <- forM blocks $ \b -> do
    pages <- forM (blockPages b) $ \p -> do
      doc <- either (failAt $ blockSource b) pure $
        resolveLinks site (Just (b,p)) (pageDoc p)
      pure p {pageDoc = doc}
    pure (blockName b,b {blockPages = pages})
  pure site {siteBlocks = M.fromList resolved}

resolveLinks :: Site -> Maybe (Block,Page) -> P.Pandoc -> Either String P.Pandoc
resolveLinks site current = walkM rewrite
  where
    rewrite (P.Link a label (url,title)) = do
      target <- resolve (T.unpack url)
      pure $ P.Link a label (T.pack target,title)
    rewrite x = Right x
    resolve url
      | Just ref <- stripPrefix "block:" url = do
          let (name,fragment) = break (== '#') ref
          b <- maybe (Left $ "unknown block link: " ++ url) Right $
                 M.lookup (unEscapeString name) (siteBlocks site)
          if null fragment then Right (toUrl $ blockPath b) else anchor b (tail fragment)
      | Just fragment <- stripPrefix "#" url, Just (b,_) <- current = anchor b fragment
      | otherwise = Right url
    anchor b fragment = do
      let idText = unEscapeString fragment
      key <- maybe (Left $ "unknown ID: block:" ++ blockName b ++ "#" ++ fragment) Right $
               M.lookup idText (blockAnchors b)
      let samePage = maybe False (\(here,p) -> blockName here == blockName b && pageKey p == key) current
          path = if samePage then "" else toUrl (blockDir b </> key <.> "html")
      Right (path ++ "#" ++ fragment)

sessionDoc :: P.ReaderOptions -> Site -> Session -> Either String P.Pandoc
sessionDoc opts site s = markdown opts (sessionIntro s) >>= resolveLinks site Nothing
