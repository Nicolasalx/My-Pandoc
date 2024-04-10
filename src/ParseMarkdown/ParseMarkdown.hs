{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
-}

module ParseMarkdown.ParseMarkdown (parseMarkdown) where
import Content (PHeader(..), PBody(..))

parseMarkdown :: String -> IO (Either String (PHeader, PBody))
parseMarkdown file = return (Right ((PHeader "" (Just "") (Just "")), PBody []))
