{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Data.Monoid
import Options.Applicative
import Control.Monad

import CAR.ToolVersion
import CAR.Types
import CAR.TocFile as Toc
import CAR.NameToIdMap as NameIdx
    ( createNameToIdMap, createRedirectToIdMap, createQidToIdMap, createRNameToQidMap )


mode :: Parser (IO ())
mode = subparser
    $  command "pages" (info (helper <*> indexPages) fullDesc)
    <> command "stubs" (info (helper <*> indexStubs) fullDesc)
    <> command "paragraphs" (info (helper <*> indexParagraphs) fullDesc)
    <> command "page-names" (info (helper <*> indexPageNames) fullDesc)
    <> command "page-redirects" (info (helper <*> indexPageRedirects) fullDesc)
    <> command "page-qids" (info (helper <*> indexPageQids) fullDesc)
    <> command "qid-names" (info (helper <*> indexQidPageNames) fullDesc)
    <> command "all" (info (helper <*> indexAll) fullDesc)
  where
    articlesFile = argument str (help "articles cbor file" <> metavar "FILE")
    indexPages =
        void . Toc.createIndex pageId <$> articlesFile
    indexStubs =
        void . Toc.createIndex stubPageId <$> argument str (help "outlines cbor file" <> metavar "FILE")
    indexParagraphs =
        void . Toc.createIndex paraId <$> argument str (help "paragraphs cbor file" <> metavar "FILE")
    indexPageNames =
        NameIdx.createNameToIdMap <$> articlesFile
    indexPageRedirects =
        NameIdx.createRedirectToIdMap <$> articlesFile
    indexPageQids =
        NameIdx.createQidToIdMap <$> articlesFile
    indexQidPageNames =
        NameIdx.createRNameToQidMap
            <$> flag False True (short 't' <> long "tsv" <> help "produce output in TSV format")
            <*> articlesFile
    indexAll =
        f <$> articlesFile
      where f inputFile = do
                void $ Toc.createIndex pageId inputFile
                NameIdx.createNameToIdMap inputFile
                NameIdx.createRedirectToIdMap inputFile
                NameIdx.createQidToIdMap inputFile
                NameIdx.createRNameToQidMap False inputFile


main :: IO ()
main = join $ execParser' 2 (helper <*> mode) mempty
