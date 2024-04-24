{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- SkipSpaces
-}

module ParseMarkdown.ParseElem.SkipSpaces (skipSpaces) where
import ParseMarkdown.DataStructMarkdown ()
import Content ()

skipSpaces :: Int -> String -> String
skipSpaces _ [] = []
skipSpaces 0 str = str
skipSpaces index (' ':xs) = skipSpaces (index-1) xs
skipSpaces _ (x:xs) = (x:xs)
