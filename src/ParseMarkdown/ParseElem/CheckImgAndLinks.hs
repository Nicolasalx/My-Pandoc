{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- checkImgAndLinks
-}

module ParseMarkdown.ParseElem.CheckImgAndLinks (checkImgAndLink) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content ()
import ParsingLib.ParseString (parseString)
import ParsingLib.ParseOneChar (parseOneChar)

checkImgAndLink :: Char -> String -> DataParsing -> (DataParsing, String)
checkImgAndLink c str dataParsing
    | Just (_, rightPart) <- parseString "![" (c : str) =
        (dataParsing { isInAltImage = True }, rightPart)
    | Just (_, rightPart) <- parseString "](" str =
        endLinkOrImg c rightPart dataParsing
    | otherwise = (dataParsing, str)

endLinkOrImg :: Char -> String -> DataParsing -> (DataParsing, String)
endLinkOrImg c str dataParsing
    | isInContentLink dataParsing == True =
        ((parseOneChar c dataParsing)
        { isInContentLink = False, isInUrlLink = True }, str)
    | isInAltImage dataParsing == True =
        ((parseOneChar c dataParsing)
        { isInAltImage = False, isInUrlImage = True }, str)
    | otherwise = (dataParsing, str)
