{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
-}

module ParseMarkdown.ParseMarkdown (parseMarkdown) where
import Content (PContent(..))
import ParseMarkdown.ParseBody (parseBody)
import ParseMarkdown.ParseHeader (parseHeader)

parseMarkdown :: String -> Either String [PContent]
parseMarkdown file_content =
    parseHeader file_content >>
    Right []
    -- parseHeader file_content
    -- parseBody file_content
    -- Right []
