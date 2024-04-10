{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
-}

module ParseMarkdown.ParseMarkdown (parseMarkdown) where
import Content (PContent(..))

-- IO is for debug purpose but while be removed after to: parseMarkdown :: String -> Either String [PContent]
parseMarkdown :: String -> IO (Either String [PContent])
parseMarkdown file = return (Right [])
