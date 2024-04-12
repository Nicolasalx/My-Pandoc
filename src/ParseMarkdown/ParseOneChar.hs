--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseOneChar
--

module ParseMarkdown.ParseOneChar(parseOneChar) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))

addCharToActualList :: Char -> DataParsing -> IO DataParsing
addCharToActualList c dataParsing
    | isInContentLink dataParsing == True = return (dataParsing { contentLink = contentLink dataParsing ++ [c] , actualList = actualList dataParsing ++ [c], nbReturnLines = 0})
    | isInAltImage dataParsing == True = return (dataParsing { altImg = altImg dataParsing ++ [c], actualList = actualList dataParsing ++ [c], nbReturnLines = 0})
    | isInUrlLink dataParsing == True = return (dataParsing { urlLink = urlLink dataParsing ++ [c], actualList = actualList dataParsing ++ [c], nbReturnLines = 0})
    | isInUrlImage dataParsing == True = return (dataParsing { urlImg = urlImg dataParsing ++ [c], actualList = actualList dataParsing ++ [c], nbReturnLines = 0})
    | otherwise = return (dataParsing { actualList = actualList dataParsing ++ [c], nbReturnLines = 0})

addBasicCharToActualList :: Char -> DataParsing -> IO DataParsing
addBasicCharToActualList c dataParsing = return (dataParsing { actualList = actualList dataParsing ++ [c], nbReturnLines = 0})

parseOneChar :: Char -> DataParsing -> IO DataParsing
parseOneChar '`' dataParsing = do -- Text Formatting -> Code (Check if we are in a paragraph)
    (addCharToActualList '`' dataParsing)

parseOneChar '[' dataParsing = addBasicCharToActualList '[' dataParsing { isInContentLink = True }

parseOneChar ')' dataParsing
    | isInUrlLink dataParsing == True = addCharToActualList ')' (dataParsing { isInUrlLink = False }) -- A link has been completely fill, now i will add in the DataStructure PContent
    | isInUrlImage dataParsing == True = addCharToActualList ')' (dataParsing { isInUrlImage = False }) -- An image has been completely fill, now i will add in the DataStructure PContent

parseOneChar '*' dataParsing = do -- Text Formatting -> Code (Check if we are in a paragraph)
    (addCharToActualList '*' dataParsing)

parseOneChar c dataParsing = do
    addCharToActualList c dataParsing
