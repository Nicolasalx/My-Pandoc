{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- CleanLine
-}

module ParsingLib.CleanLine (cleanLine) where

import Data.Char (isSpace)

cleanLine :: String -> Maybe String
cleanLine [] = Nothing
cleanLine str = Just $ dropWhile isSpace str
