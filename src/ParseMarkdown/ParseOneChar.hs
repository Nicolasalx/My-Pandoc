--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseOneChar
--

module ParseMarkdown.ParseOneChar (parseOneChar) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content ()
import ParseMarkdown.LinksAndImages.Links (insertLinkToParagraph)
import ParseMarkdown.LinksAndImages.Image (insertImageToParagraph)

addCharToActualList :: Char -> DataParsing -> IO DataParsing
addCharToActualList c dataParsing
    | isInContentLink dataParsing == True = return (dataParsing { contentLink = contentLink dataParsing ++ [c], nbReturnLines = 0})
    | isInAltImage dataParsing == True = return (dataParsing { altImg = altImg dataParsing ++ [c], nbReturnLines = 0})
    | isInUrlLink dataParsing == True = return (dataParsing { urlLink = urlLink dataParsing ++ [c], nbReturnLines = 0})
    | isInUrlImage dataParsing == True = return (dataParsing { urlImg = urlImg dataParsing ++ [c], nbReturnLines = 0})
    | otherwise = return (dataParsing { actualList = actualList dataParsing ++ [c], nbReturnLines = 0})

addBasicCharToActualList :: Char -> DataParsing -> IO DataParsing
addBasicCharToActualList c dataParsing = return (dataParsing { actualList = actualList dataParsing ++ [c], nbReturnLines = 0})

parseOneChar :: Char -> DataParsing -> IO DataParsing
-- ! Verify Link or Image
-- parseOneChar '[' dataParsing = addBasicCharToActualList '[' dataParsing { isInContentLink = True }
parseOneChar '[' dataParsing = return (dataParsing { isInContentLink = True, nbReturnLines = 0 })

parseOneChar ')' dataParsing
    | isInUrlLink dataParsing = do
        newDataParsed <- return (dataParsing { isInUrlLink = False })
        (insertLinkToParagraph newDataParsed)
    | isInUrlImage dataParsing = do
        newDataParsed <- return (dataParsing { isInUrlImage = False })
        (insertImageToParagraph newDataParsed)
    | otherwise = addBasicCharToActualList '[' dataParsing { isInContentLink = True }

parseOneChar c dataParsing = addCharToActualList c dataParsing
