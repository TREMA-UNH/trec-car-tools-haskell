{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CAR.NameToIdMap
    (
    createNameToIdMap
    , openNameToIdMap
    , pageNameToIdMaybeSet
    , pageNameToIdSet
    , pageNameToAnId
    , pageNamesToIdSet
    , openRedirectToIdMap
    , openRNameToQidMap
    , openQidToIdMap
    , qidToIdMaybeSet
    , createRedirectToIdMap
    , createQidToIdMap
    , createRNameToQidMap
    , NameToIdMap (..)
    , NameToQidMap (..)
    , QidToIdMap (..)
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import CAR.Types.Files
import CAR.Types
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as BSL
import System.FilePath
import Data.Maybe
import CAR.Utils (getWikidataQid)

newtype NameToIdMap = NameToIdMap (M.Map PageName (S.Set PageId))
                    deriving (CBOR.Serialise)

newtype NameToQidMap = NameToQidMap (M.Map PageName (S.Set WikiDataId))
                    deriving (CBOR.Serialise)

newtype QidToIdMap = QidToIdMap (M.Map WikiDataId (S.Set PageId))
                    deriving (CBOR.Serialise)


buildInfoToIdMap :: (Page -> [PageName]) -> FilePath -> IO (NameToIdMap)
buildInfoToIdMap pageToInfo cborPath = do
    (_, pages) <- readPagesOrOutlinesAsPagesWithProvenance cborPath
    return
        $ NameToIdMap
        $ M.fromListWith (<>)
        $ [ (name, S.singleton (pageId page))
          | page <- pages
          , name <- pageToInfo page
          ]
createInfoToIdMap :: (Page -> [PageName]) -> String -> FilePath -> IO ()
createInfoToIdMap transform extension cborPath = do
    index <- buildInfoToIdMap transform cborPath
    BSL.writeFile indexPath $ CBOR.serialise index
  where indexPath = cborPath <.> extension


buildRInfoToQidMap :: (Page -> [PageName]) -> FilePath -> IO (NameToQidMap)
buildRInfoToQidMap pageToInfo cborPath = do
    (_, pages) <- readPagesOrOutlinesAsPagesWithProvenance cborPath
    return
        $ NameToQidMap
        $ M.fromListWith (<>)
        $ [ (name, S.singleton qid)
          | page <- pages
          , name <- pageToInfo page
          , Just qid <- pure $ getWikidataQid page
          ]

createRInfoToQidMap :: (Page -> [PageName]) -> String -> FilePath -> IO ()
createRInfoToQidMap transform extension cborPath = do
    index <- buildRInfoToQidMap transform cborPath
    BSL.writeFile indexPath $ CBOR.serialise index
  where indexPath = cborPath <.> extension




buildQidToIdMap :: (Page -> Maybe WikiDataId) -> FilePath -> IO (QidToIdMap)
buildQidToIdMap pageToQid cborPath = do
    (_, pages) <- readPagesOrOutlinesAsPagesWithProvenance cborPath
    return
        $ QidToIdMap
        $ M.fromListWith (<>)
        $ [ (qid, S.singleton (pageId page))
          | page <- pages
          , Just qid <- pure $ pageToQid page
          ]



createQidToIdMap' :: (Page -> Maybe WikiDataId) -> String -> FilePath -> IO ()
createQidToIdMap' transform extension cborPath = do
    index <- buildQidToIdMap transform cborPath
    BSL.writeFile indexPath $ CBOR.serialise index
  where indexPath = cborPath <.> extension


openInfoToIdMap :: String -> FilePath -> IO NameToIdMap
openInfoToIdMap extension cborPath = do
    index <- either onError snd . CBOR.Read.deserialiseFromBytes CBOR.decode
           <$> BSL.readFile indexPath
    return index
  where
    indexPath = cborPath <.> extension
    onError err =
        error $ "Deserialisation error while deserialising TOC "++show indexPath++": "++show err

openInfoToIdMap' :: String -> FilePath -> IO QidToIdMap
openInfoToIdMap' extension cborPath = do
    index <- either onError snd . CBOR.Read.deserialiseFromBytes CBOR.decode
           <$> BSL.readFile indexPath
    return index
  where
    indexPath = cborPath <.> extension
    onError err =
        error $ "Deserialisation error while deserialising TOC "++show indexPath++": "++show err

openRNameToQidMap' :: String -> FilePath -> IO NameToQidMap
openRNameToQidMap' extension cborPath = do
    index <- either onError snd . CBOR.Read.deserialiseFromBytes CBOR.decode
           <$> BSL.readFile indexPath
    return index
  where
    indexPath = cborPath <.> extension
    onError err =
        error $ "Deserialisation error while deserialising TOC "++show indexPath++": "++show err


openNameToIdMap :: FilePath -> IO NameToIdMap
openNameToIdMap = openInfoToIdMap "name"
openRedirectToIdMap :: FilePath -> IO NameToIdMap
openRedirectToIdMap = openInfoToIdMap "redirect"
openQidToIdMap :: FilePath -> IO QidToIdMap
openQidToIdMap = openInfoToIdMap' "qid"

openRNameToQidMap :: FilePath -> IO NameToQidMap
openRNameToQidMap = openRNameToQidMap' "qid2name"



createNameToIdMap :: FilePath -> IO ()
createNameToIdMap = createInfoToIdMap  (\p -> [pageName p]) "name"

createRedirectToIdMap :: FilePath -> IO ()
createRedirectToIdMap = createInfoToIdMap  page2redirect  "redirect"

createRNameToQidMap :: FilePath -> IO ()
createRNameToQidMap = createRInfoToQidMap  (\p -> [pageName p] <> page2redirect p) "qid2name"


page2redirect :: Page -> [PageName]
page2redirect page = (pageName page) : (fromMaybe [] $ getMetadata _RedirectNames (pageMetadata page))


createQidToIdMap :: FilePath -> IO ()
createQidToIdMap = createQidToIdMap'  page2qid  "qid"
  where
    page2qid :: Page -> Maybe WikiDataId
    page2qid page = getMetadata _WikiDataQID (pageMetadata page)



pageNameToIdMaybeSet :: NameToIdMap -> PageName -> Maybe (S.Set PageId)
pageNameToIdMaybeSet (NameToIdMap m) name =
    name `M.lookup` m

qidToIdMaybeSet :: QidToIdMap -> WikiDataId -> Maybe (S.Set PageId)
qidToIdMaybeSet (QidToIdMap m) qid =
    qid `M.lookup` m

pageNameToIdSet :: NameToIdMap -> PageName -> (S.Set PageId)
pageNameToIdSet (NameToIdMap m) name =
    fromMaybe S.empty $ name `M.lookup` m

pageNameToAnId :: NameToIdMap -> PageName -> Maybe PageId
pageNameToAnId (NameToIdMap m) name =
    case name `M.lookup` m of
        Nothing -> Nothing
        Just xs | S.null xs -> Nothing
        Just xs -> Just (head $ S.toList xs)


pageNamesToIdSet :: NameToIdMap -> [PageName] -> S.Set PageId
pageNamesToIdSet m = foldMap (pageNameToIdSet m)
