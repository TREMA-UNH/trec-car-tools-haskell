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


mode :: Parser (IO ())
mode = subparser
    $  command "pages" (info (helper <*> indexPages) fullDesc)
    <> command "stubs" (info (helper <*> indexStubs) fullDesc)
    <> command "paragraphs" (info (helper <*> indexParagraphs) fullDesc)
    <> command "page-names" (info (helper <*> indexPageNames) fullDesc)
    <> command "page-redirects" (info (helper <*> indexPageRedirects) fullDesc)
  where
    indexPages =
        void . Toc.createIndex pageId <$> argument str (help "articles cbor file" <> metavar "FILE")
    indexStubs =
        void . Toc.createIndex stubPageId <$> argument str (help "outlines cbor file" <> metavar "FILE")
    indexParagraphs =
        void . Toc.createIndex paraId <$> argument str (help "paragraphs cbor file" <> metavar "FILE")
    indexPageNames =
        void . NameIdx.createNameToIdMap <$> argument str (help "articles cbor file" <> metavar "FILE")
    indexPageRedirects =
        void . NameIdx.createRedirectToIdMap <$> argument str (help "articles cbor file" <> metavar "FILE")


main :: IO ()
main = join $ execParser' 2 (helper <*> mode) mempty
