{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}


module CAR.CarJSON
    ( -- * Aeson FromJSON and ToJSON for CAR.AST data types
      S(..)
    , unwrapS
    ) where

import Data.Monoid hiding (All, Any)
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson

import CAR.Types

t :: T.Text -> T.Text
t = id

t_PARAGRAPH = t "paragraph"
t_SECTION = t "section"
t_LIST = t "list"
t_IMAGE = t "image"
t_INFOBOX = t "infobox"

k_TYPE = "type"
k_LINK = "link"
k_TEXT = "text"
k_KEY = "key"
k_VALUE = "value"
k_FREQ = "freq"
k_TARGET_PAGE = "target_page"
k_TARGET_PAGE_ID = "target_page_id"
k_TARGET_SECTION  = "target_section"
k_PARA_ID = "para_id"
k_PARA_BODY = "para_body"
k_HEADING_TEXT = "heading_text"
k_HEADING_ID = "heading_id"
k_SKELETON = "content"
k_PARAGRAPH = "paragraph"
k_INDENT = "indent"
k_ENTRY = "entry"
k_IMAGE_FILE = "image_file"
k_IMAGE_CAPTION = "image_caption"
k_INFOBOX_NAME = "infobox_name"
k_INFOBOX_ENTRIES = "infobox_entries"
k_REDIRECT_NAMES = "redirect_names"
k_DISAMBIGUATION_NAMES = "disambiguation_names"
k_DISAMBIGUATION_IDS = "disambiguation_ids"
k_CATEGORY_NAMES = "category_names"
k_CATEGORY_IDS = "category_ids"
k_INLINK_IDS = "inlink_ids"
k_PAGE_TAGS = "page_tags"
k_OLD_INLINK_ANCHORS = "old_inlink_anchors"
k_INLINK_ANCHORS = "inlink_anchors"
k_WIKIDATA_QID = "wiki_data_qid"
k_WIKI_SITE_ID = "wiki_site_id"
k_PAGE_NAME = "page_name"
k_PAGE_ID = "page_id"
k_PAGE_TYPE = "page_type"
k_REDIRECT_TARGET = "redirect_target"
k_METADATA = "metadata"

k_PROV_SITE_PROVENANCES = "site_provenances"
k_PROV_RELEASE_NAME = "release_name"
k_PROV_COMMENTS = "comments"
k_PROV_SITE_ID = "site_id"
k_PROV_LANGUAGE = "language"
k_PROV_SOURCE_NAME = "source_name"
k_PROV_SITE_COMMENTS = "site_comments" 


-- | Serialized newtype for JSON representation
newtype S a = S a
    deriving (Show)

deriving instance Aeson.ToJSON a => Aeson.ToJSON (S [a])
deriving instance Aeson.FromJSON a => Aeson.FromJSON (S [a])

unwrapS :: Functor f => f (S a) -> f a
unwrapS xs = fmap (\(S a) -> a) xs


instance Aeson.ToJSON (S Link) where
    toJSON (S Link {..})=
        object
        $ [ k_TEXT .= linkAnchor
         , k_TARGET_PAGE .= linkTarget
         , k_TARGET_PAGE_ID .= linkTargetId
         ] <>
         maybe [] (\l -> [ k_TARGET_SECTION .= l]) linkSection


instance Aeson.FromJSON (S Link) where
    parseJSON = Aeson.withObject "S Link" $ \content -> do
        linkAnchor <- content Aeson..: k_TEXT
        linkTarget <- content Aeson..: k_TARGET_PAGE
        linkTargetId <- content Aeson..: k_TARGET_PAGE_ID
        linkSection <- content Aeson..:? k_TARGET_SECTION
        return $ S (Link {..})

-- --------- Infobox ------------------
data InfoboxEntry = InfoboxEntry { infoboxKey :: T.Text
                                 , infoboxValue :: [PageSkeleton] 
                                 }


instance Aeson.ToJSON (S InfoboxEntry) where 
    toJSON (S InfoboxEntry{..}) =
        object 
        $ [ k_KEY .= infoboxKey
          , k_VALUE .= fmap S infoboxValue
          ] 

instance Aeson.FromJSON (S InfoboxEntry) where
    parseJSON = Aeson.withObject "S InfoboxEntry" $ \content -> do
        infoboxKey <- content Aeson..: k_KEY
        infoboxValue <- unwrapS <$> content Aeson..: k_VALUE
        return $ S (InfoboxEntry {..})

-- ---------- Paragraph and ParaBody -------------

instance Aeson.ToJSON (S ParaBody) where 
    toJSON (S (ParaText txt)) = object [k_TEXT .= txt]
    toJSON (S (ParaLink Link {..})) =
        object
        $ [ k_TEXT .= linkAnchor
         , k_TARGET_PAGE .= linkTarget
         , k_TARGET_PAGE_ID .= linkTargetId
         ] <>
         maybe [] (\l -> [ k_TARGET_SECTION .= l]) linkSection


instance Aeson.FromJSON (S ParaBody) where
    parseJSON = Aeson.withObject "S ParaBody" $ \content -> do
        txt <- content Aeson..: k_TEXT
        maybeTargetPage <- content Aeson..:? k_TARGET_PAGE
        case maybeTargetPage of
            Nothing -> return $ S (ParaText txt)
            Just linkTarget -> do
                linkTargetId <- content Aeson..: k_TARGET_PAGE_ID
                linkSection <- content Aeson..:? k_TARGET_SECTION
                return $ S (ParaLink (Link {linkAnchor=txt, ..} ))


instance Aeson.ToJSON (S Paragraph) where 
    toJSON (S Paragraph {..}) =
        object
        $ [ k_PARA_ID .= unpackParagraphId paraId
          , k_PARA_BODY.= fmap S paraBody
          ]

instance Aeson.FromJSON (S Paragraph) where
    parseJSON = Aeson.withObject "S Paragraph" $ \content -> do
        paraId <- packParagraphId <$> content Aeson..: k_PARA_ID
        paraBody <- unwrapS <$> content Aeson..: k_PARA_BODY
        return $ S (Paragraph {..})

-- --------- PageSkeleton -----------------

instance Aeson.ToJSON (S PageSkeleton) where 
    toJSON (S (Section (SectionHeading headingText) headingId sectionSkeleton)) =
        object 
        $ [ k_TYPE .= t_SECTION
          , k_HEADING_TEXT .= headingText
          , k_HEADING_ID .= unpackHeadingId headingId
          , k_SKELETON .=  fmap S sectionSkeleton
          ]
    toJSON (S (Para paragraph )) =
        object 
        $ [ k_TYPE .= t_PARAGRAPH
          , k_PARAGRAPH .= S paragraph
          ]
    toJSON (S (List indent paragraph )) =
        object 
        $ [ k_TYPE .= t_LIST
          , k_INDENT .= indent
          , k_ENTRY .= S paragraph
          ]
    toJSON (S (Image imageFile caption)) =
        object 
        $ [ k_TYPE .= t_IMAGE
          , k_IMAGE_FILE .= imageFile
          , k_IMAGE_CAPTION .= fmap S caption
          ]
    toJSON (S (Infobox infoboxName infoboxEntries)) =
        object 
        $ [ k_TYPE .= t_INFOBOX
          , k_INFOBOX_NAME .= infoboxName
          , k_INFOBOX_ENTRIES .=
                fmap (\(key,skel) -> S (InfoboxEntry key skel)) infoboxEntries
          ]

instance Aeson.FromJSON (S PageSkeleton) where
    parseJSON = Aeson.withObject "S PageSkeleton" $ \content -> do
        skelType <- content Aeson..: k_TYPE
        if | skelType == t_SECTION -> do
                headingText <- content Aeson..: k_HEADING_TEXT
                headingId <- packHeadingId <$> content Aeson..: k_HEADING_ID
                sectionSkeleton <- unwrapS <$>  content Aeson..: k_SKELETON
                return $ S (Section (SectionHeading headingText) headingId sectionSkeleton)
           | skelType == t_PARAGRAPH -> do
                S paragraph <- content Aeson..: k_PARAGRAPH
                return $ S (Para paragraph)
           | skelType == t_LIST -> do
                S paragraph <- content Aeson..: k_ENTRY
                indent <- content Aeson..: k_INDENT
                return $ S (List indent paragraph)
           | skelType ==  t_IMAGE -> do
                caption <- unwrapS <$> content Aeson..: k_IMAGE_CAPTION
                imageFile <- content Aeson..: k_IMAGE_FILE
                return $ S (Image imageFile caption)
           | skelType ==  t_INFOBOX -> do
                infoboxName <- content Aeson..: k_INFOBOX_NAME
                infoboxEntries <- fmap (\(S (InfoboxEntry key skel)) -> (key,skel)) <$> content Aeson..: k_INFOBOX_ENTRIES
                return $ S (Infobox infoboxName infoboxEntries)
           | otherwise -> error $ "Don't know how to parse PageSkeleton of type " <> show skelType
 
-- -------- PageMetadata -------------

instance Aeson.ToJSON (S PageMetadata) where
    toJSON (S (PageMetadata metadataItems)) =
        object $ fmap metadataItemToJSON metadataItems
      
metadataItemToJSON :: MetadataItem ->  (T.Text, Value) -- Aeson.Pair
metadataItemToJSON (RedirectNames pageNames) =
    k_REDIRECT_NAMES .= pageNames
metadataItemToJSON (DisambiguationNames pageNames) =
    k_DISAMBIGUATION_NAMES .= pageNames
metadataItemToJSON (DisambiguationIds pageIds) =
    k_DISAMBIGUATION_IDS .= pageIds
metadataItemToJSON (CategoryNames pageNames) =
    k_CATEGORY_NAMES .= pageNames
metadataItemToJSON (CategoryIds pageIds) =
    k_CATEGORY_IDS .= pageIds
metadataItemToJSON (InlinkIds pageIds) =
    k_INLINK_IDS .= pageIds
metadataItemToJSON (PageTags tags) =
    k_PAGE_TAGS .= tags

metadataItemToJSON (OldInlinkAnchors texts) =
    k_OLD_INLINK_ANCHORS .= texts
metadataItemToJSON (InlinkAnchors anchorStats) =
    k_INLINK_ANCHORS .= fmap anchorStatToJSON anchorStats

metadataItemToJSON (WikiDataQID wikiDataId) =
    k_WIKIDATA_QID .= wikiDataId
metadataItemToJSON (WikiSiteId siteId) =
    k_WIKI_SITE_ID .= siteId

metadataItemToJSON m@(UnknownMetadata _a _b _terms) = error $ "Can produce JSON for UnknownMetadata items: "<> show m

anchorStatToJSON :: (T.Text, Int) -> Aeson.Value
anchorStatToJSON (anchor, freq) =
    object $ [ k_TEXT .= anchor, k_FREQ .= freq ]



instance Aeson.FromJSON (S PageMetadata) where
    parseJSON = Aeson.withObject "S PageMetadata" $ \content -> do
        let parseMeta :: FromJSON a =>  (a -> MetadataItem) -> T.Text -> Aeson.Parser (Maybe MetadataItem)
            parseMeta constr field =
                fmap constr <$> content Aeson..:? field

        redirectNames <-  parseMeta RedirectNames k_REDIRECT_NAMES
        disambiguationNames <- parseMeta DisambiguationNames k_DISAMBIGUATION_NAMES
        disambiguationIds <- parseMeta DisambiguationIds k_DISAMBIGUATION_IDS
        categoryNames <- parseMeta CategoryNames k_CATEGORY_NAMES
        categoryIds <- parseMeta CategoryIds k_CATEGORY_IDS
        inlinkIds <- parseMeta InlinkIds  k_INLINK_IDS
        pageTags <- parseMeta PageTags k_PAGE_TAGS

        oldInlinkAnchors <- parseMeta OldInlinkAnchors k_OLD_INLINK_ANCHORS
        inlinkAnchors <- do
            -- the field k_INLINK_ANCHORS may or may not be present, which is why we use :?
            -- maybeAnchors  set to `Just anchorStatsEntry ` if field was present
            -- The JSON representation of anchorStatsEntry is `{anchor="text", freq=5}`
            -- we convert these to a list of `(T.Text,Int)` using `anchorStatFromJSON`
            -- finally we wrap the result in `InlinkAnchors`
            maybeAnchors <- content Aeson..:? k_INLINK_ANCHORS
                        --  :: Aeson.Parser (Maybe (V.Vector Aeson.Value))
            case maybeAnchors of
                Just anchors ->  Just . InlinkAnchors <$> mapM anchorStatFromJSON anchors
                Nothing -> return Nothing

        wikiDataQid <- parseMeta WikiDataQID k_WIKIDATA_QID
        wikiSiteId <- parseMeta WikiSiteId k_WIKI_SITE_ID

        return $ S (PageMetadata 
                      $ catMaybes [ redirectNames
                                  , disambiguationNames
                                  , disambiguationIds
                                  , categoryNames
                                  , categoryIds
                                  , inlinkIds
                                  , oldInlinkAnchors
                                  , inlinkAnchors
                                  , wikiDataQid
                                  , wikiSiteId
                                  , pageTags
                                  ] )

anchorStatFromJSON :: Aeson.Value -> Aeson.Parser (T.Text, Int)
anchorStatFromJSON =
    Aeson.withObject "anchorStatFromJSON" $ \content -> do
        anchor <- content Aeson..: k_TEXT
        freq <- content Aeson..: k_FREQ
        return $ (anchor, freq)  

-- --------------- Page -----------

pageTypeToJSON :: PageType -> Aeson.Value
pageTypeToJSON ArticlePage = "article"
pageTypeToJSON CategoryPage = "category"
pageTypeToJSON DisambiguationPage = "disambiguation"
pageTypeToJSON (RedirectPage _link) = "redirect" -- also exposes field "redirect_target"


instance Aeson.ToJSON (S Page) where
    toJSON (S Page{..}) =
        object
        $ [ k_PAGE_NAME .= pageName
            , k_PAGE_ID .= pageId
            , k_PAGE_TYPE .= pageTypeToJSON pageType
        ] 
        ++ [ k_REDIRECT_TARGET .= S link | RedirectPage link <- pure pageType ]
        ++ [ k_METADATA .= S pageMetadata
           , k_SKELETON .= map S pageSkeleton
           ]


instance Aeson.FromJSON (S Page) where
    parseJSON = Aeson.withObject "S Page" $ \content-> do
        pageName <- content Aeson..: k_PAGE_NAME
        pageId <- content Aeson..: k_PAGE_ID
        pageType <- do
            ty <- content Aeson..: k_PAGE_TYPE
            case ty :: T.Text of
                "article" -> pure ArticlePage
                "category" -> pure CategoryPage
                "disambiguation" -> pure DisambiguationPage
                "redirect" -> do S link <- content Aeson..: "redirect_target"
                                 return $ RedirectPage link
                _ -> error $ "Don't know how to parse page type "<> show ty                 

        S pageMetadata <- content Aeson..: k_METADATA
        pageSkeleton <- fmap (\(S x) -> x)  <$> content Aeson..: k_SKELETON

        return $ S (Page {..})


----------- Provenance ------

instance Aeson.ToJSON (S SiteProvenance) where
    toJSON (S SiteProvenance{language = Language language', provSiteId = SiteId provSiteId', ..}) =
        object
        $ [ k_PROV_SITE_ID .= provSiteId'
            , k_PROV_LANGUAGE .= language'
            , k_PROV_SOURCE_NAME .= sourceName
            , k_PROV_SITE_COMMENTS .= siteComments       
           ] 
   

instance Aeson.FromJSON (S SiteProvenance) where
    parseJSON = Aeson.withObject "S SiteProvenance" $ \content -> do
        provSiteId' <-  content Aeson..: k_PROV_SITE_ID
        language' <- content Aeson..: k_PROV_LANGUAGE
        sourceName <- content Aeson..: k_PROV_SOURCE_NAME
        siteComments <- content Aeson..: k_PROV_SITE_COMMENTS

        return $ S (SiteProvenance {language = Language language'
                                   ,provSiteId = SiteId provSiteId'
                                   , ..})

instance Aeson.ToJSON (S Provenance) where
    toJSON (S Provenance{..}) =
        object
            [ k_PROV_SITE_PROVENANCES .= fmap S siteProvenances
            , k_PROV_RELEASE_NAME .= dataReleaseName
            , k_PROV_COMMENTS .= comments
            ] 
   

instance Aeson.FromJSON (S Provenance) where
    parseJSON = Aeson.withObject "S Provenance" $ \content -> do
        siteProvenances <- unwrapS <$>  content Aeson..: k_PROV_SITE_PROVENANCES
        dataReleaseName <- content Aeson..: k_PROV_RELEASE_NAME
        comments <- content Aeson..: k_PROV_COMMENTS

        return $ S (Provenance {transforms = [], ..})


