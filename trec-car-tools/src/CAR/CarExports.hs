{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CAR.CarExports
    ( PassageFile
      -- * Stubs
    , Stub(..)
    , toStubSkeleton
    , prettyStub
      -- * Paragraphs
    , toParagraphs
      -- * Ground truth
    , Relevance
    , IsRelevant(..)
    , Annotation(..)
    , EntityAnnotation(..)
    , EntityPassageAnnotation(..)
    , toAnnotations
    , toEntityAnnotations
    , toEntityPassageAnnotations
    ) where

import Data.Maybe
import qualified Data.DList as DList
import qualified Data.Text as T
import qualified Data.Set as S

-- import Data.MediaWiki.Markup (PageName(..))
import CAR.Types hiding (paraId)
import CAR.Utils
import CAR.QRelFile

-- Passage file
type PassageFile = [Paragraph]
type Relevance = IsRelevant

toStubSkeleton :: Page -> Stub
toStubSkeleton (Page name pageId ty meta skeleton) =
    Stub name pageId ty meta (mapMaybe go skeleton)
  where
    go :: PageSkeleton -> Maybe PageSkeleton
    go (Section heading headingId children) =
        Just $ Section heading headingId (mapMaybe go children)
    go (Para _)    = Nothing
    go (Image{})   = Nothing
    go (List{})    = Nothing
    go (Infobox{}) = Nothing

prettyStub :: Stub -> String
prettyStub (Stub (PageName name) _ _ _ skeleton) =
    unlines $ [ T.unpack name, replicate (T.length name) '=', "" ]
           ++ map (prettySkeleton anchorOnly) skeleton

toParagraphs :: Page -> [Paragraph]
toParagraphs =
    concatMap go . pageSkeleton
  where
    go :: PageSkeleton -> [Paragraph]
    go (Section _ _ children) = concatMap go children
    go (Para para) = [para]
    go (Image{})   = [] -- ignore images
    go (List{})    = []
    go (Infobox{}) = []

toAnnotations :: Page -> S.Set (Annotation IsRelevant)
toAnnotations (Page _ pageId _ _ skeleton) =
    -- recurse into sections, recursively collect section path, emit one annotation per paragraph
    foldMap (go mempty) skeleton
  where
    go :: DList.DList HeadingId -> PageSkeleton -> S.Set (Annotation IsRelevant)
    go parentIds (Section _ sectId children) =
        let parentIds' = parentIds `DList.snoc` sectId
        in foldMap (go parentIds') children
    go parentIds (Para (Paragraph paraId _)) =
        S.singleton $ Annotation sectionPath paraId Relevant
      where
        sectionPath = SectionPath pageId (DList.toList parentIds)
    go _parentIds (Image{})  = mempty
    go _parentIds (List{})   = mempty
    go _parentIds (Infobox{})= mempty

toEntityAnnotations :: Page ->  S.Set (EntityAnnotation IsRelevant)
toEntityAnnotations (Page { pageId, pageSkeleton = skeleton }) =
    -- recurse into sections, recursively collect section path, emit one entity annotation per link
    foldMap (go mempty) skeleton
  where
    go :: DList.DList HeadingId -> PageSkeleton -> S.Set (EntityAnnotation IsRelevant)
    go parentIds (Section _ sectionId children) =
        let parentIds' = parentIds `DList.snoc` sectionId
        in foldMap (go parentIds') children
    go parentIds (Para paragraph) =
        let entityIds = filter (not . badEntityId)
                      $ fmap linkTargetId
                      $ paraLinks paragraph
        in S.fromList
            $  [EntityAnnotation sectionPath entityId Relevant
               | entityId <- entityIds
               ]
      where
        sectionPath = SectionPath pageId (DList.toList parentIds)
        badEntityId entityId = null $ unpackPageId entityId
    go _parentIds (Image{})   = mempty
    go _parentIds (List{})    = mempty
    go _parentIds (Infobox{}) = mempty

toEntityPassageAnnotations :: Page ->  S.Set (EntityPassageAnnotation IsRelevant)
toEntityPassageAnnotations (Page { pageId, pageSkeleton = skeleton }) =
    -- recurse into sections, recursively collect section path, emit one entity annotation per link
    foldMap (go mempty) skeleton
  where
    go :: DList.DList HeadingId -> PageSkeleton -> S.Set (EntityPassageAnnotation IsRelevant)
    go parentIds (Section _ sectionId children) =
        let parentIds' = parentIds `DList.snoc` sectionId
        in foldMap (go parentIds') children
    go parentIds (Para paragraph@(Paragraph paraId _)) =
        let entityIds = filter (not . badEntityId)
                      $ fmap linkTargetId
                      $ paraLinks paragraph
        in S.fromList
            $  [EntityPassageAnnotation sectionPath entityId paraId Relevant
               | entityId <- entityIds
               ]
      where
        sectionPath = SectionPath pageId (DList.toList parentIds)
        badEntityId entityId = null $ unpackPageId entityId
    go _parentIds (Image{})   = mempty
    go _parentIds (List{})    = mempty
    go _parentIds (Infobox{}) = mempty
