{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AddBasicCharToActualList
-}

module ParsingLib.AddBasicCharToActualList (addBasicCharToActualList) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))

addBasicCharToActualList :: Char -> DataParsing -> DataParsing
addBasicCharToActualList c dataParsing =
    (dataParsing {
        actualList = actualList dataParsing ++ [c], nbReturnLines = 0})