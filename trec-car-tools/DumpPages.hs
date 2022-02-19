{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where
import Control.Monad
import Data.List (partition, intersperse, intercalate)
import Data.Maybe

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Options.Applicative
    ( Alternative((<|>), many),
      optional,
      Parser,
      argument,
      command,
      flag,
      fullDesc,
      help,
      info,
      long,
      metavar,
      option,
      short,
      str,
      subparser,
      switch,
      helper )
import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified CAR.TocFile as TocFile
import qualified CAR.AnnotationsFile as CAR
import CAR.Types
    ( Page(pageName, pageSkeleton, pageId, pageMetadata, Page),
      Paragraph(paraId),
      SectionHeading(getSectionHeading),
      PageName(getPageName),
      unpackPageName,
      packPageName,
      packPageId,
      unpackPageId,
      unpackParagraphId,
      packParagraphId,
      readWikiDataId,
      _RedirectNames,
      _WikiDataQID,
      getMetadata,
      escapeSectionPath,
      anchorOnly,
      prettyMeta,
      prettyPage,
      prettyParagraph,
      prettySkeleton,
      withLink,
      readParagraphsFile )
import CAR.Utils
import CAR.ToolVersion
import qualified CAR.Types.AST as CAR
    ( WikiDataId (WikiDataId),
      Page,
      PageSkeleton(..),
      ParagraphId,
      Paragraph(Paragraph),
      SectionHeading(SectionHeading),
      PageId,
      PageName(PageName), MetadataItem (WikiDataQID) )
import qualified CAR.Types.AST.Pretty as CAR ( LinkStyle )
import qualified CAR.Types.Files as CAR
    ( readPagesOrOutlinesAsPages, Header )
import qualified Data.Text.IO as DataTextIO
import System.IO (hPutStrLn, stderr)
import Prelude hiding (negate)
import qualified Codec.Serialise.Decoding as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Map as M
import CAR.NameToIdMap (NameToQidMap (..))

opts :: Parser (IO ())
opts = subparser
    $  cmd "titles"        dumpTitles
    <> cmd "page-ids"      dumpPageIds
    <> cmd "page-qids"     dumpPageQids
    <> cmd "meta"          dumpMeta
    <> cmd "pages"         dumpPages
    <> cmd "pages-corpus"  dumpPagesCorpus
    <> cmd "entityids"     dumpEntityIds
    <> cmd "paragraphs"    dumpParagraphs
    <> cmd "paragraph-corpus" dumpParagraphCorpus
    <> cmd "paragraphids"  dumpParagraphIds
    <> cmd "filter-paragraphids"  filterParagraphIds
    <> cmd "paragraphids-pages"  paragraphIdsInPages
    <> cmd "section-ids"      dumpSectionIds
    <> cmd "outlines"      dumpOutlines
    <> cmd "hist-headings" histogramHeadings
    <> cmd "dump-header"   dumpHeader
    <> cmd "provenance"   dumpHeader
    <> cmd "queries"  dumpQueries
    <> cmd "infobox"  dumpInfoboxes
    <> cmd "convert-page-ids"  dumpConvertPageIds
    <> cmd "future-convert-page-ids"  dumpFutureConvertPageIds
    -- <> cmd "convert-page-qids"  dumpConvertWikidataQIDs
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    dumpHeader =
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f inputFile = do
            hdr <- CBOR.deserialise <$> BSL.readFile inputFile
            print (hdr :: CAR.Header)

    dumpTitles =
        f <$> pagesFromFile
      where
        f getPages = do
            pages <- getPages
            mapM_ (T.putStrLn . getPageName . pageName) pages

    dumpPageIds =
        f <$> pagesFromFile
      where
        f getPages = do
            pages <- getPages
            mapM_ (putStrLn . unpackPageId . pageId) pages

    dumpPageQids =
        f <$> pagesFromFile
      where
        f getPages = do
            pages <- getPages
            forM_ pages $ \ (Page{pageMetadata = meta, pageId = pid}) -> do
                let maybeQid = getMetadata _WikiDataQID meta
                         :: Maybe CAR.WikiDataId
                case maybeQid of
                  Just qid -> print qid
                  Nothing  -> hPutStrLn stderr $ "Page " <> unpackPageId pid <> " has no Wikidata QID"

    dumpSectionIds =
        f <$> pagesFromFile
          <*> flag False True (long "raw" <> help "only section paths - no pagenames")
          <*> flag False True (long "internal" <> help "also internal sections")
      where
        f getPages raw internal = do
            pages <- getPages
            let sectionpathlist p = fmap escapeSectionPath
                                  $ pageSectionPaths p
                pageNameStr p = (T.unpack $ getPageName $ pageName p)

            if raw then
                mapM_  (\p -> putStrLn $ unlines $ sectionpathlist p) pages
            else
                mapM_ (\p -> putStrLn $ unlines $ pageNameStr p : sectionpathlist p) pages

    dumpQueries =
        f <$> pagesFromFile
          <*> flag False True (long "section" <> help "enable for section queries (otherwise page queries)")
      where
        f getPages useSection = do
            pages <- getPages
            if useSection then
              mapM_ (\p -> putStrLn $ unlines $ sectionQueries p ) pages
            else
              mapM_ (\p -> putStrLn $ pageQueries p) pages

        pageQueries :: CAR.Page -> String
        pageQueries page =
          let qid = unpackPageId $ pageId page
              qtext = unpackPageName $ pageName page
          in qid <> "\t" <> qtext

        sectionQueries :: CAR.Page -> [String]
        sectionQueries page  =
          [ escapeSectionPath sectionPath <> "\t"
            <> (unpackPageName $ pageName page)
            <> " " <> T.unpack (T.intercalate " " (fmap getSectionHeading headingList) )
          | (sectionPath, headingList, _) <- pageSections page
          ]


    dumpOutlines =
        f <$> pagesFromFile
          <*> flag False True (long "page-id" <> help "also print page id")
      where
        f getPages withPageId = do
            pages <- getPages

            let indentHeadings p = [ (length headinglist, last headinglist)
                                   |  (_, headinglist, _) <- pageSections p
                                   , headinglist /= []
                                   ]  -- [(SectionPath, [SectionHeading], [PageSkeleton])]

                pageNameStr p = (T.unpack $ getPageName $ pageName p)
                pageIdStr p = (unpackPageId $ pageId p)

                formatIndentHeadings :: (Int, CAR.SectionHeading) -> String
                formatIndentHeadings (level, headingtext) = (replicate level '\t') <> T.unpack (getSectionHeading headingtext)


                prettyPage p = unlines $
                    ( if withPageId then [pageIdStr p] else [] )
                    ++ [ pageNameStr p ]
                    ++ fmap formatIndentHeadings (indentHeadings p)
                    ++ [""]

            putStrLn $ unlines $ map prettyPage pages


    dumpPages =
        f <$> pagesFromFile
          <*> flag anchorOnly withLink (long "links" <> help "Show link targets")
      where
        f :: IO [CAR.Page] -> CAR.LinkStyle -> IO ()
        f getPages linkStyle = do
            pages <- getPages
            putStrLn $ unlines $ map (prettyPage linkStyle) pages


    dumpInfoboxes =
        f <$> pagesFromFile

      where
        f :: IO [CAR.Page] -> IO ()
        f getPages = do
            pages <- getPages
            mapM_ (TL.putStrLn . prettyInfoBoxes) pages
          where
            prettyInfoBoxes :: CAR.Page -> TL.Text
            prettyInfoBoxes page =
                TL.unlines $ mapMaybe (infoboxToText page) (pageInfoboxes page)

            pageInfoboxes :: CAR.Page -> [CAR.PageSkeleton]
            pageInfoboxes = foldMap pageSkeletonInfobox . pageSkeleton

            pageSkeletonInfobox :: CAR.PageSkeleton -> [CAR.PageSkeleton]
            pageSkeletonInfobox (CAR.Section _ _ children) = foldMap pageSkeletonInfobox children
            pageSkeletonInfobox (CAR.Para paragraph) = []
            pageSkeletonInfobox (CAR.Image {}) = []
            pageSkeletonInfobox (CAR.List _ paragraph) = []
            pageSkeletonInfobox box@(CAR.Infobox tag args) = [box]


            infoboxToText :: CAR.Page -> CAR.PageSkeleton -> Maybe TL.Text
            infoboxToText page (CAR.Infobox  title keyValues) = Just $ TL.unlines $
                [ ""
                , "Page:" <> (TL.pack $ unpackPageName $ pageName page)
                , "[" <> TL.fromStrict title  <> "]" ]
                ++ fmap toText keyValues
              where toText :: (T.Text, [CAR.PageSkeleton]) -> TL.Text
                    toText (key, skels) =
                      let key' :: TL.Text
                          key' = TL.fromStrict key
                          vals' :: [TL.Text]
                          vals' =  fmap (TL.pack . (prettySkeleton withLink)) $ skels
                      in key' <> " = " <> TL.strip (TL.concat vals')
              -- where toText (ParaText text) = TL.fromStrict text
              --       toText (ParaLink link) = TL.fromStrict $ linkAnchor link
            infoboxToText _ _ = Nothing



    dumpPagesCorpus =
        f <$> pagesFromFile
      where
        f :: IO [CAR.Page] -> IO ()
        f getPages = do
            pages <- getPages
            mapM_ dumpPage pages
          where
            dumpPage :: CAR.Page -> IO ()
            dumpPage page =
                TL.writeFile fname $ TL.unlines $ map paraToText (pageParas page)
              where
                fname = unpackPageName $ pageName page


    dumpMeta =
        f <$> pagesFromFile
      where
        f :: IO [CAR.Page] -> IO ()
        f getPages = do
            pages <- getPages
            putStrLn $ unlines $ map prettyMeta pages

    paragraphIdsInPages =
        f <$> pagesFromFile
      where
        f :: IO [CAR.Page] -> IO ()
        f getPages = do
            pages <- getPages
            putStrLn $ unlines
              [ unpackParagraphId paraId'
              | page <- pages
              , CAR.Paragraph paraId' _ <- pageParas page
              ]

    dumpEntityIds =
        f <$> pagesFromFile
      where
        f :: IO [CAR.Page] -> IO ()
        f getPages = do
                pages <- getPages
                putStrLn $ unlines $ map entityIdFromPage pages

          where
            entityIdFromPage page =
              let pname = unpackPageName $ pageName page
                  pid = unpackPageId $ pageId page
              in pid <> "\t" <> pname

    dumpParagraphs =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
          <*> flag anchorOnly withLink (long "links" <> help "Show link targets")
          <*> many (option (packParagraphId <$> str) (long "paragraph" <> short 'p' <> help "dump only paragraphs with the given paragraph id"))
      where
        f :: FilePath -> CAR.LinkStyle -> [CAR.ParagraphId] -> IO ()
        f inputFile linkStyle paraIds = do
            paragraphs <- if null paraIds
              then readParagraphsFile inputFile
              else do paras <- TocFile.open $ TocFile.IndexedCborPath inputFile
                      return $ mapMaybe (`TocFile.lookup` paras) paraIds
            putStrLn $ unlines $ map (prettyParagraph linkStyle) paragraphs

    dumpParagraphCorpus =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
      where
        f :: FilePath -> IO ()
        f inputFile = do
            paragraphs <- readParagraphsFile inputFile
            let fmtPara para = TL.pack (unpackParagraphId $ paraId para) <> "\t" <> paraToText para
            TL.putStrLn $ TL.unlines $ map fmtPara paragraphs

    dumpParagraphIds =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
      where
        f :: FilePath -> IO ()
        f inputFile  = do
                paragraphs <- readParagraphsFile inputFile
                putStrLn $ unlines $ map (unpackParagraphId . paraId) paragraphs

    filterParagraphIds =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
          <*> option (TocFile.IndexedCborPath <$> str) (long "para2" <> help "dump only paragraph ids that are also in this file")
          <*> switch (short 'n' <> long "negate" <> help "invert matching logic")
      where
        f :: FilePath -> TocFile.IndexedCborPath CAR.ParagraphId CAR.Paragraph -> Bool -> IO ()
        f inputFile para2File negate = do
                paragraphs <- readParagraphsFile inputFile
                para2 <- TocFile.open para2File
                let paragraphs' = filter (\ (CAR.Paragraph pid _) ->  negate /= isJust (TocFile.lookup pid para2) ) paragraphs
                putStrLn $ unlines $ map (unpackParagraphId . paraId) paragraphs'

    histogramHeadings =
        f <$> pagesFromFile
      where
        f getPages = do
            pages <- getPages
            TL.putStrLn $ TB.toLazyText
                $ mconcat
                $ intersperse (TB.singleton '\n')
                $ map (\(CAR.SectionHeading h, n) -> TB.decimal n<>TB.singleton '\t'<>TB.fromText h)
                $ HM.toList
                $ HM.fromListWith (+)
                $ map (\h -> (h,1::Int))
                $ foldMap sectionHeadings
                $ foldMap pageSkeleton pages


-- readFilteredPagesHs :: HS.HashSet CAR.PageName    -- ^ set of page names to read
--                   -> HS.HashSet CAR.PageId    -- ^ set of page names to read
--                   -> HS.HashSet CAR.WikiDataId
--                   -> CAR.PageBundle          -- ^ pages or outlines file
--                   -> [CAR.Page]
-- readFilteredPagesHs pageNames pageIds qids pageBundle =
--    if HS.null pageNames && HS.null pageIds && HS.null qids  then
--      CAR.bundleAllPages pageBundle
--    else
--      let pageIds' = S.filter (\pid -> not $ HS.member pid pageIds )  -- drop duplicates
--                    $ (CAR.bundleLookupAllPageNames pageBundle) pageNames
--      in mapMaybe (CAR.bundleLookupPage pageBundle) ( (HS.toList  pageIds) <> (S.toList pageIds')  )

readFilteredPages :: S.Set CAR.PageName    -- ^ set of page names to read
                  -> S.Set CAR.PageId    -- ^ set of page names to read
                  -> HS.HashSet CAR.PageName
                  -> S.Set CAR.WikiDataId
                  -> CAR.PageBundle          -- ^ pages or outlines file
                  -> [CAR.Page]
readFilteredPages pageNames pageIds redirects qids pageBundle =
   if S.null pageNames && S.null pageIds && S.null qids  then
     CAR.bundleAllPages pageBundle
   else
     let pageIds' = S.unions ([
                   pageIds
                  , (CAR.bundleLookupAllPageNames pageBundle) pageNames
                  , (CAR.bundleLookupAllWikidataQids pageBundle) qids
                  ] <>
                  mapMaybe (CAR.bundleLookupRedirect pageBundle) (HS.toList redirects)
                  )
     in mapMaybe (CAR.bundleLookupPage pageBundle) (S.toList  pageIds')

pagesFromFile :: Parser (IO [CAR.Page])
pagesFromFile = allPagesFromFile <|> filteredPagesFromFile

allPagesFromFile :: Parser (IO [CAR.Page])
allPagesFromFile =
    f <$> argument str (help "input file" <> metavar "FILE")
  where
    f :: FilePath -> IO [CAR.Page]
    f inputFile = do
        CAR.readPagesOrOutlinesAsPages inputFile


filteredPagesFromFile :: Parser (IO [CAR.Page])
filteredPagesFromFile =
    f <$> argument str (help "input file" <> metavar "FILE")
      <*> fmap S.fromList (many (option  (packPageName  <$> str) (short 'p' <> long "page" <> metavar "PAGE NAME" <> help "Page name to dump or nothing to dump all")))
      <*> optional (option str (long "pagenames-from-file" <> metavar "FILE" <> help "like --page but reads all entries from a file instead of being passed in as arguments"))
      <*> fmap S.fromList (many (option  (packPageId  <$> str) (short 'P' <> long "pageid" <> metavar "PAGE ID " <> help "Page id to dump or nothing to dump all")))
      <*> many (option (packPageName <$> str) (long "target" <> short 't' <> help "dump only pages with links to this target page name (and the page itself)"))      <*> ( HS.fromList <$> many (option (packPageId <$> str) (long "targetids" <> short 'T'  <> help "dump only pages with links to this target page id (and the page itself)")))
      <*> ( HS.fromList <$> many (option (packPageName <$> str)  (long "redirect" <> short 'r' <> help "dump only pages with redirects from this page name")))
      <*> optional (option str (long "redirects-from-file" <> metavar "FILE" <> help "like --redirect but reads all entries from a file instead of being passed in as arguments"))
      <*> ( S.fromList <$> many (option readWikiDataId' (long "qid" <> short 'q' <> help "dump only pages with this qid")))
      <*> optional (option str (long "qids-from-file" <> metavar "FILE" <> help "like --qid but reads all entries from a file instead of being passed in as arguments"))
  where
    readWikiDataId' = do
      s <- str
      case readWikiDataId s of
        Nothing -> fail $ "Invalid WikiDataId: " <> T.unpack s
        Just x -> return x

    f :: FilePath
      -> S.Set CAR.PageName
      -> Maybe FilePath
      -> S.Set CAR.PageId -> [CAR.PageName]
      -> HS.HashSet CAR.PageId
      -> HS.HashSet CAR.PageName
      -> Maybe FilePath
      -> S.Set CAR.WikiDataId
      -> Maybe FilePath
      -> IO [CAR.Page]
    f inputFile pageNames pageNamesFile pageIds targetPageNames1 targetPageIds2 redirectPageNames redirectsFile qids qidsFile= do
        pageBundle <- CAR.openPageBundle inputFile
        pageNames' <-
          case pageNamesFile of
            Just f ->  S.fromList <$> fromFile convPageName f
            Nothing -> return pageNames

        redirectTargets' <-
          case redirectsFile of
            Just f ->  HS.fromList <$> fromFile convPageName f
            Nothing -> return redirectPageNames

        qidTargets' <-
          case qidsFile of
            Just f ->  S.fromList . catMaybes <$> fromFile convQid f
            Nothing -> return qids
              :: IO (S.Set CAR.WikiDataId)

        let targetPageIds1 = S.toList $ CAR.bundleLookupAllPageNames pageBundle targetPageNames1

            targetPageIds =
                HS.fromList targetPageIds1
                `HS.union` targetPageIds2

            searchTargets :: HS.HashSet CAR.PageId -> CAR.Page -> Bool
            searchTargets targets page =
                if | HS.null targets -> True
                   | pageId page `HS.member` targets -> True
                   | otherwise     -> let pageTargets = HS.fromList (pageLinkTargetIds page)
                                      in any (  `HS.member` pageTargets) targets
            redirectTargets :: HS.HashSet CAR.PageName -> CAR.Page -> Bool
            redirectTargets redirects page =
                if | HS.null redirects -> True
                   | Just pageRedirects <- getMetadata _RedirectNames (pageMetadata page)
                        -> let pageRedirectSet = HS.fromList pageRedirects
                           in any (  `HS.member` pageRedirectSet) redirects
                   | otherwise -> False

            qidTargets :: S.Set CAR.WikiDataId -> CAR.Page -> Bool
            qidTargets qids page =
                if | S.null qids -> True
                   | Just qid <- getMetadata _WikiDataQID (pageMetadata page)
                        -> qid `S.member` qids
                   | otherwise -> False

            pages = readFilteredPages pageNames' pageIds redirectTargets' qidTargets' pageBundle
        return -- $ filter (qidTargets qidTargets')
               -- $ filter (redirectTargets redirectTargets')
               $ filter (searchTargets targetPageIds) pages



    fromFile :: (BSL.ByteString -> a ) ->  FilePath -> IO [a]
    fromFile conv filePath = do
        contents <- BSL.readFile filePath
                 :: IO BSL.ByteString
        return $ map conv $ BSL.lines contents

    convPageName :: (BSL.ByteString -> PageName )
    convPageName str =  packPageName $ TL.unpack $ TL.decodeUtf8 str

    convQid :: (BSL.ByteString -> Maybe CAR.WikiDataId )
    convQid  str = readWikiDataId  $ TL.toStrict $ TL.decodeUtf8 $ str


sectionHeadings ::CAR.PageSkeleton -> [CAR.SectionHeading]
sectionHeadings (CAR.Section h _ children) = h : foldMap sectionHeadings children
sectionHeadings _ = []




dumpConvertPageIds :: Parser (IO ())
dumpConvertPageIds =
    f <$> argument str (help "input file" <> metavar "CBOR-FILE")
      <*> argument str (help "file with page ids for conversion (one line per page id)" <> metavar "ID-File")
  where
    f :: FilePath -> FilePath -> IO ()
    f inputFile redirectPageFile = do
        pageIdsToFind <- map CAR.PageName . T.lines <$> DataTextIO.readFile redirectPageFile
        pageBundle <- CAR.openPageBundle inputFile

        let result = fmap (convertPageIds pageBundle) pageIdsToFind
        hPutStrLn stderr $ unlines [ msg | Left msg <- result]
        putStrLn $ unlines [ unpackPageId foundPageId <> "\t"<> unpackPageName oldPageName  | Right (foundPageId, oldPageName) <- result]

    convertPageIds :: CAR.PageBundle -> CAR.PageName -> Either String (CAR.PageId, CAR.PageName)
    convertPageIds pageBundle pageName =
      case CAR.bundleLookupPageName pageBundle pageName of
        Just pageIdSet | not $ S.null pageIdSet
          -> let newPageId = head $ S.toList pageIdSet
             in Right $ (newPageId, pageName)
        Nothing ->
          case CAR.bundleLookupRedirect pageBundle pageName of
            Just pageIdSet | not $ S.null pageIdSet
              -> let newPageId = head $ S.toList pageIdSet
                in Right $ (newPageId, pageName)
            _ -> Left $ "Not found: "<> show pageName
        _ ->
            Left $ "No page information available"



-- dumpConvertWikidataQIDs :: Parser (IO ())
-- dumpConvertWikidataQIDs =
--     f <$> argument str (help "input file" <> metavar "CBOR-FILE")
--       <*> argument str (help "file with page names for conversion (one line per page)" <> metavar "Name-File")
--   where
--     f :: FilePath -> FilePath -> IO ()
--     f inputFile nameFile = do
--         pageNamesToFind <- map CAR.PageName . T.lines <$> DataTextIO.readFile nameFile
--         pageBundle <- CAR.openPageBundle inputFile

--         let result = fmap (convertQIDs pageBundle) pageNamesToFind
--         hPutStrLn stderr $ unlines [ msg | Left msg <- result]
--         putStrLn $ unlines 
--                  [ unpackPageName oldPageName <> "\t"<> (intercalate "\t" (fmap show qids))
--                  | Right (oldPageName, qids) <- result
--                  ]

--     convertQIDs :: CAR.PageBundle -> CAR.PageName -> Either String (CAR.PageName, [CAR.WikiDataId])
--     convertQIDs pageBundle pageName =
--       let --name2qid :: (M.Map PageName (S.Set WikiDataId) -- NameToQidMap
--           (NameToQidMap name2qid) = CAR.bundleNameToQidLookup pageBundle 

--       in case pageName `M.lookup` name2qid of
--             Just qidIdSet | not $ S.null qidIdSet
--               -> Right $ (pageName, S.toList qidIdSet)
--             -- Nothing ->
--             --   case CAR.bundleLookupRedirect pageBundle pageName of
--             --     Just pageIdSet | not $ S.null pageIdSet
--             --       -> let qid = head $ S.toList pageIdSet
--             --         in Right $ (qid, pageName)
--             --     _ -> Left $ "Not found: "<> show pageName
--             _ ->
--                 Left $ "No qid information available for page name "<> show pageName


dumpFutureConvertPageIds :: Parser (IO())
dumpFutureConvertPageIds =
    f <$> option str (long "bundle" <> metavar "CBOR" <> help "filepath to CBOR, matching toc, names, and redirects must exist. These can be created with `trec-car-build-toc`.")
      <*> optional (option str (long "future-bundle" <> metavar "CBOR" <> help "filepath to CBOR of a future dump (e.g. 2020), matching toc, names, and redirects must exist."))
      <*> argument str ( metavar "TITLE-FILE" <> help "File with wikipedia titles to be converted. One line per title, text encoding must be UTF-8")
      <*> option str (short 'o' <> metavar "OUT-FILE" <> help "Output file, where output will be written to. Format is `entityid \\t given page title`")
  where
    f :: FilePath -> Maybe FilePath -> FilePath -> FilePath -> IO()
    f wiki16InputFile wiki20InputFileOpt titlesToConvert outputFile = do
        bundle16 <- CAR.openPageBundle wiki16InputFile
        titleList <- fmap (packPageName . TL.unpack) <$> TL.lines <$> TL.readFile titlesToConvert

        let (lookups16,titleList')
                      = partition (isJust . snd)
                      $ look bundle16 titleList
            missingTitles2016 = fmap fst titleList'
        putStrLn $ "Found "<> (show $ length lookups16) <> " pages in given CBOR"

        -- only do this if the future bundle is given
        (converted16, missingTitles) <- do
              case wiki20InputFileOpt of
                Nothing -> return ([],missingTitles2016)
                Just wiki20InputFile -> do
                      bundle20 <- CAR.openPageBundle wiki20InputFile
                      let (lookups20,missingTitles')
                                    = partition (isJust . snd)
                                    $ look bundle20 missingTitles2016

                          missingTitles = fmap fst missingTitles'

                      putStrLn $ "Found "<> (show $ length lookups20) <> " pages in given future CBOR"

                      let converted16 =
                            [ (orig, maybeSet page16IdSet)
                              | (orig, Just page20IdSet) <-lookups20
                              , let page16IdSet = downgradePageId bundle16 bundle20 page20IdSet
                              ]
                            where maybeSet :: S.Set a -> Maybe (S.Set a)
                                  maybeSet set =
                                    if S.null set
                                      then Nothing
                                      else Just set
                          (converted16', missingConverted) = partition (isJust . snd) converted16
                          missingConverted' = fmap fst missingConverted

                      putStrLn $ "Converted "<> (show $ length converted16') <> " from future CBOR pages"
                      putStrLn ""
                      T.putStrLn $ "Pages found in future bundle, but not in target bundle: " <> (T.unlines $ fmap (T.pack . unpackPageName ) missingConverted')
                      putStrLn ""

                      return $ (converted16', missingTitles)

        T.putStrLn $ "Page titles unresolvable in future or target bundle: " <> (T.unlines $ fmap (T.pack . unpackPageName ) missingTitles)

        let total16 = lookups16 <> converted16

        putStrLn $ ""
        TL.writeFile outputFile
          $  TL.unlines
          $ [ (TL.pack $ unpackPageId id) <> "\t" <> (TL.pack $ unpackPageName orig)
            | (orig, Just ids) <- total16, id <- S.toList ids
            ]


    look :: CAR.PageBundle -> [CAR.PageName] -> [(CAR.PageName, Maybe (S.Set CAR.PageId))]
    look bundle titleList =
      [ (title, titleOpt <|> redirectOpt )
        | title <- titleList
        , let titleOpt = CAR.bundleLookupPageName bundle title
              redirectOpt = CAR.bundleLookupRedirect bundle title
      ]

    downgradePageId :: CAR.PageBundle -> CAR.PageBundle -> S.Set CAR.PageId -> S.Set CAR.PageId
    downgradePageId bundle16 bundle20 pageIds20 =
          S.unions
            $ [ CAR.bundleLookupAllPageNames bundle16
                $ fromMaybe [] $ getMetadata _RedirectNames $ pageMetadata  page20

            | pageId20 <- S.toList pageIds20
            , Just page20 <- pure $ CAR.bundleLookupPage bundle20 pageId20
            ]


main :: IO ()
main = join $ execParser' 1 (helper <*> opts) mempty
