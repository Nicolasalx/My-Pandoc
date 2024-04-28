{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- checkImgAndLinks
-}

module ParseMarkdown.ParseElem.CheckImgAndLinks (checkImgAndLink) where
import Content ()
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import ParsingLib.ParseOneChar (parseOneChar)
import ParsingLib.ParseString (parseString, runParser)

checkImgAndLink :: Char -> String -> DataParsing -> (DataParsing, String)
checkImgAndLink c str dataParsing
    | Just (_, rightPart) <- runParser (parseString "![") (c : str) =
        (dataParsing { isInAltImage = True }, rightPart)
    | Just (_, rightPart) <- runParser (parseString "](") str =
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
