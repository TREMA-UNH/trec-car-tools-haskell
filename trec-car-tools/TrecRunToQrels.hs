{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Monoid
import Options.Applicative
import Control.Monad
import Control.DeepSeq
import Data.Maybe
import Data.Hashable
import Codec.Serialise as CBOR
import Data.List
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import CAR.Types
import CAR.TocFile as Toc
import CAR.ToolVersion

import CAR.RunFile as CAR
import CAR.QRelFile as CAR
import SimplIR.Format.TrecRunFile as TrecRunFile
import SimplIR.Format.QRel as Qrel
import qualified SimplIR.Ranking as Ranking


mode :: Parser (IO ())
mode = subparser
    $  command "page-level" (info (helper <*> pageLevelRuns) fullDesc)
    <>  command "section-level" (info (helper <*> sectionLevelRuns) fullDesc)
  where
    outputRunFile = option str (long "output" <> short 'o' <> help "Output run file" <> metavar "RUNFILE")
    inputRunFile = argument str (help "Input run file" <> metavar "RUNFILE")
    outlinesFile = option str (help "Outlines files" <> short 's' <> long "outlines" <> metavar "STUB")
    pageLevelRuns =
        pageLevelRuns2Qrels  stringToPageId
            <$> inputRunFile
            <*> outputRunFile

    sectionLevelRuns =
        sectionLevelRuns2Qrels stringToSectionHeading
            <$> inputRunFile
            <*> outlinesFile
            <*> outputRunFile

    stringToSectionHeading pageIds name =
        head $ mapMaybe (\pi -> split pi name) pageIds
      where split :: PageId -> T.Text -> Maybe SectionPath
            split pageId name =
                    let pageId' = unpackPageId pageId
                        name' = T.unpack name
                    in if pageId' `isPrefixOf` name'
                        then
                            let heading  = drop (length pageId') name'
                            in Just $ SectionPath {sectionPathPageId=pageId, sectionPathHeadings = [packHeadingId heading] }
                        else Nothing    

    stringToPageId _pageIds name =
        SectionPath { sectionPathPageId = packPageId $ T.unpack name, sectionPathHeadings = [] }


pageLevelRuns2Qrels :: ([PageId] -> TrecRunFile.QueryId -> SectionPath) -> FilePath -> FilePath -> IO ()
pageLevelRuns2Qrels stringToSectionPath inputRunFile outputRunFile = do
    rankings <- readParagraphRun inputRunFile
    let filteredQrels :: [CAR.Annotation IsRelevant]
        filteredQrels = 
            [ CAR.Annotation sectionPath paragraphId Relevant
            |  CAR.RankingEntry{carRank=rank,carQueryId=qId, carDocument=paragraphId,..} <- rankings
            , rank <= 50
            , let sectionPath = stringToSectionPath [] $ unQueryId qId
            ]
    writeParagraphQRel outputRunFile filteredQrels


instance Hashable a => Hashable (Annotation a) where
    hashWithSalt salt (Annotation sectionPath paraId isRel)  =
        hashWithSalt salt (sectionPath, paraId, isRel)


sectionLevelRuns2Qrels :: ([PageId] -> TrecRunFile.QueryId -> SectionPath) -> FilePath -> FilePath -> FilePath -> IO ()
sectionLevelRuns2Qrels stringToSectionPath inputRunFile outlinesFile outputRunFile = do
    outlines <- readOutlinesFile outlinesFile
    let titles =
            [ queryId
            | Stub{stubPageId=queryId} <- outlines
            ]
    rankings <- readParagraphRun inputRunFile
    let aggregateQrel :: [(PageId, [(CAR.Annotation IsRelevant, Double)])]
        aggregateQrel = 
            [ ( titleQuery, [(annotation, 1.0/ realToFrac rank)])
            |  CAR.RankingEntry{carRank=rank ,carQueryId=qId, carDocument=paragraphId, ..} <- rankings
            , rank <= 50
            , let SectionPath titleQuery _heading = stringToSectionPath titles $ unQueryId qId
                  titlePath = SectionPath titleQuery []
            , let annotation = CAR.Annotation titlePath paragraphId Relevant
            ]
        aggregatedQrelScores =
            concat
            [ Ranking.toSortedList rankedQrels
                | (_key, entryList) <-  HM.toList  $ HM.fromListWith (++) aggregateQrel
                , let aggregateScores = HM.fromListWith (+) entryList
                , let rankedQrels = Ranking.fromListK 50 $ fmap (\(a,b) -> (b,a)) $ HM.toList aggregateScores
            ]
        

    writeParagraphQRel outputRunFile $ map snd aggregatedQrelScores

main :: IO ()
main = join $ execParser' 1 (helper <*> mode) mempty
