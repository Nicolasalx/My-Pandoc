{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
-}

module ParseMarkdown.ParseMarkdown (parseMarkdown) where

parseMarkdown :: String -> IO () -- return value to be define
parseMarkdown file = putStrLn ("parse markdown:\n" ++ file)
