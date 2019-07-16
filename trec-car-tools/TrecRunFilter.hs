{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Monoid
import Options.Applicative
import Control.Monad
import Control.DeepSeq
import Data.Maybe
import Data.Hashable
import Codec.Serialise as CBOR
import qualified Data.Text as T

import CAR.Types
import CAR.TocFile as Toc
import CAR.ToolVersion

import SimplIR.Format.TrecRunFile as TrecRunFile


mode :: Parser (IO ())
mode = subparser
    $  command "pages" (info (helper <*> indexPages) fullDesc)
    <> command "paragraphs" (info (helper <*> indexParagraphs) fullDesc)
  where
    outputRunFile = option str (long "output" <> short 'o' <> help "Output run file" <> metavar "RUNFILE")
    inputRunFile = argument str (help "Input run file" <> metavar "RUNFILE")
    indexPages =
        filterRankings docNameToTocKey
            <$> argument (IndexedCborPath @PageId @Page <$> str)
                         (help "pages file" <> metavar "FILE")
            <*> inputRunFile
            <*> outputRunFile
      where docNameToTocKey =
              packPageId .  T.unpack

    indexParagraphs =
        filterRankings docNameToTocKey
            <$> argument (IndexedCborPath @ParagraphId @Paragraph <$> str)
                         (help "paragraphs file" <> metavar "FILE")
            <*> inputRunFile
            <*> outputRunFile
      where docNameToTocKey =
              packParagraphId . T.unpack


filterRankings :: (CBOR.Serialise i, Hashable i, Eq i, NFData i, CBOR.Serialise a)
               => (TrecRunFile.DocumentName -> i) -> IndexedCborPath i a -> FilePath -> FilePath -> IO ()
filterRankings docNameToTocKey indexPath inputRunFile outputRunFile = do
    index <- Toc.open indexPath
    rankings <- readRunFile inputRunFile :: IO [RankingEntry]
    let filteredRankings = filter (\RankingEntry{..} -> isJust $ (docNameToTocKey documentName) `Toc.lookup` index ) rankings
    writeRunFile outputRunFile filteredRankings

main :: IO ()
main = join $ execParser' 1 (helper <*> mode) mempty
