{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AppendElemToList
-}

module ParsingLib.AppendElemToList (appendElemToList) where

import ParseMarkdown.DataStructMarkdown (DataParsing(..))

appendElemToList :: Char -> DataParsing -> DataParsing
appendElemToList c dataParsing =
    (dataParsing {
        actualList = actualList dataParsing ++ [c], nbReturnLines = 0})
