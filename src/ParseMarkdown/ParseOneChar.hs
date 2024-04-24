{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseOneChar
-}

module ParseMarkdown.ParseOneChar (parseOneChar) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content ()
import ParseMarkdown.LinksAndImages.Links (insertLinkToParagraph)
import ParseMarkdown.LinksAndImages.Image (insertImageToParagraph)

appendElemToList :: Char -> DataParsing -> DataParsing
appendElemToList c dataParsing = (dataParsing {
            actualList = actualList dataParsing ++ [c], nbReturnLines = 0})

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

addBasicCharToActualList :: Char -> DataParsing -> DataParsing
addBasicCharToActualList c dataParsing =
    (dataParsing {
        actualList = actualList dataParsing ++ [c], nbReturnLines = 0})

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
