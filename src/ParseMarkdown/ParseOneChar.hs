--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseOneChar
--

module ParseMarkdown.ParseOneChar (parseOneChar) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PParagraphType(..), PText(..), PTextType(..), PLink(..), PImage(..))

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
parseOneChar '`' dataParsing = addCharToActualList '`' dataParsing -- For text formatting but maybe now unused

parseOneChar '[' dataParsing = addBasicCharToActualList '[' dataParsing { isInContentLink = True }

parseOneChar ')' dataParsing
    | isInUrlLink dataParsing == True = do
        newDataParsed <- addCharToActualList ')' (dataParsing { isInUrlLink = False })
        return (insertLinkToParagraph newDataParsed)
    | isInUrlImage dataParsing == True = do
        newDataParsed <- addCharToActualList ')' (dataParsing { isInUrlImage = False })
        return (insertImageToParagraph newDataParsed)
    | otherwise = addBasicCharToActualList '[' dataParsing { isInContentLink = True }

parseOneChar '*' dataParsing = addCharToActualList '*' dataParsing -- For text formatting but maybe now unused

parseOneChar c dataParsing = addCharToActualList c dataParsing

------------------------------------------------------------------------------------------------------------
-----------------------------------               LINK                 -------------------------------------
------------------------------------------------------------------------------------------------------------

insertLinkToParagraph :: DataParsing -> DataParsing
insertLinkToParagraph dataParsing
    | length (actualList dataParsing) > 0 = do
        let newDataParsed = createText dataParsing
        insertLink newDataParsed
    | otherwise = insertLink dataParsing

insertLink :: DataParsing -> DataParsing
insertLink dataParsing = do
    let newLink = formattingLink dataParsing
    dataParsing { paragraph = (paragraph dataParsing) ++ [newLink], urlLink = "", contentLink = "" }

formattingLink :: DataParsing -> PParagraphType
formattingLink dataParsing = do
    let textFormatted = formattingText (contentLink dataParsing)
    (PLinkParagraph (PLink (urlLink dataParsing) textFormatted))

createText :: DataParsing -> DataParsing
createText dataParsing = do
    let newTextType = formattingElemParagraph dataParsing
    dataParsing { paragraph = (paragraph dataParsing) ++ [newTextType] }

formattingElemParagraph :: DataParsing -> PParagraphType
formattingElemParagraph dataParsing = do
    -- take "actualString dataParsing" and formatte it
    let textFormatted = formattingText (actualList dataParsing)
    (PTextParagraph textFormatted)

formattingText :: String -> PText
formattingText str = do
    (PText [(PString (str))])

------------------------------------------------------------------------------------------------------------
-----------------------------------               IMAGE                 ------------------------------------
------------------------------------------------------------------------------------------------------------

insertImageToParagraph :: DataParsing -> DataParsing
insertImageToParagraph dataParsing
    | length (actualList dataParsing) > 0 = do
        let newDataParsed = createText dataParsing
        insertImage newDataParsed
    | otherwise = insertImage dataParsing

insertImage:: DataParsing -> DataParsing
insertImage dataParsing = do
    let newImage = formattingImg dataParsing
    dataParsing { paragraph = (paragraph dataParsing) ++ [newImage], urlImg = "", altImg = "" }

formattingImg :: DataParsing -> PParagraphType
formattingImg dataParsing = do
    let textFormatted = formattingText (altImg dataParsing)
    (PImageParagraph (PImage (urlImg dataParsing) textFormatted))
