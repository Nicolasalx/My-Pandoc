{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseOneChar
-}

module ParsingLib.ParseOneChar (parseOneChar) where
import Content ()
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import ParseMarkdown.LinksAndImages.Image (insertImageToParagraph)
import ParseMarkdown.LinksAndImages.Links (insertLinkToParagraph)
import ParsingLib.AddBasicCharToActualList (addBasicCharToActualList)
import ParsingLib.AddCharToActualList (addCharToActualList)

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
