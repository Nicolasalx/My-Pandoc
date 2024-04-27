{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AddCharToActualList
-}

module ParsingLib.AddCharToActualList (addCharToActualList) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content ()
import ParsingLib.AppendElemToList (appendElemToList)

addCharToActualList :: Char -> DataParsing -> DataParsing
addCharToActualList c dataParsing
    | isInContentLink dataParsing == True = (dataParsing {
            contentLink = contentLink dataParsing ++ [c], nbReturnLines = 0})
    | isInAltImage dataParsing == True = (dataParsing {
            altImg = altImg dataParsing ++ [c], nbReturnLines = 0})
    | isInUrlLink dataParsing == True = (dataParsing {
            urlLink = urlLink dataParsing ++ [c], nbReturnLines = 0})
    | isInUrlImage dataParsing == True = (dataParsing {
            urlImg = urlImg dataParsing ++ [c], nbReturnLines = 0})
    | otherwise = appendElemToList c dataParsing