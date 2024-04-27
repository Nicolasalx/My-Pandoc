{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseOneChar
-}

module ParsingLib.ParseOneChar (parseOneChar) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content ()
import ParseMarkdown.LinksAndImages.Links (insertLinkToParagraph)
import ParseMarkdown.LinksAndImages.Image (insertImageToParagraph)
import ParsingLib.AddCharToActualList (addCharToActualList)
import ParsingLib.AddBasicCharToActualList (addBasicCharToActualList)

parseOneChar :: Char -> DataParsing -> DataParsing
parseOneChar '[' dataParsing =
    (dataParsing { isInContentLink = True, nbReturnLines = 0 })

parseOneChar ')' dataParsing
    | isInUrlLink dataParsing = 
        insertLinkToParagraph (dataParsing { isInUrlLink = False })
    | isInUrlImage dataParsing = 
        insertImageToParagraph (dataParsing { isInUrlImage = False })
    | otherwise = 
        addBasicCharToActualList '[' dataParsing { isInContentLink = True }

parseOneChar c dataParsing = addCharToActualList c dataParsing