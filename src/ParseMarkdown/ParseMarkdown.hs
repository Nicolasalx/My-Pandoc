--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
--

module ParseMarkdown.ParseMarkdown (parseMarkdown) where
import Content (PHeader(..), PBody(..))
import ParseMarkdown.ParseBody (parseBody)
import ParseMarkdown.ParseHeader (parseHeader)
import ParseMarkdown.DataStructMarkdown (initializeDataParsing)

-- THIS FUNCTION WILL CHANGE DON T WORRY !

parseMarkdown :: String -> Either String (PHeader, PBody)
parseMarkdown file_content =
    let dataInitialized = initializeDataParsing
        allLines = lines file_content
        (headerResult, newDataParsing) = parseHeader allLines dataInitialized
    in case parseBody newDataParsing of
        Right pBody ->
            case headerResult of
                Right pHeader -> Right (pHeader, pBody)
                Left err -> Left err
        Left err -> Left err
