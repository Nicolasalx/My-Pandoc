{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
-}

module ParseMarkdown.ParseMarkdown (parseMarkdown) where
import Content (PHeader(..), PBody(..))
import ParseMarkdown.ParseBody (parseBody)
import ParseMarkdown.ParseHeader (parseHeader)
import ParseMarkdown.DataStructMarkdown (initializeDataParsing)

parseMarkdown :: String -> Either String (PHeader, PBody)
parseMarkdown file_content
    | Right pBody <- parseBody newDataParsing,
      Right pHeader <- headerResult = Right (pHeader, pBody)
    | Left err <- parseBody newDataParsing = Left err
    | Left err <- headerResult = Left err
    where
        dataInitialized = initializeDataParsing
        allLines = lines file_content
        (headerResult, newDataParsing) = parseHeader allLines dataInitialized
