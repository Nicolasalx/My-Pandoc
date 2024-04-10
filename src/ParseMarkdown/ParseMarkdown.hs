{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
-}

module ParseMarkdown.ParseMarkdown (parseMarkdown) where
import Content (PHeader(..), PBody(..))
-- import ParseMarkdown.ParseBody (parseBody)
import ParseMarkdown.ParseHeader (parseHeader)

parseMarkdown :: String -> Either String (PHeader, PBody)
parseMarkdown file_content = do
    let allLines = lines file_content
        pHeader = parseHeader allLines -- Delete first n strings you take in the header and give to the parseBody
    -- parseBody allLines

    Right ((PHeader "" (Just "") (Just "")), PBody [])
