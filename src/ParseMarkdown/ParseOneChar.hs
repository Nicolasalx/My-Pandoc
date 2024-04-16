--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseOneChar
--

module ParseMarkdown.ParseOneChar (parseOneChar, createText) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..), TypeText(..), initializeDataText, DataText(..), ElemTextType(..))
import Content (PParagraphType(..), PText(..), PTextType(..), PLink(..), PImage(..))
import ParsingLib.Lib (parseString, strcmp)

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
        (insertLinkToParagraph newDataParsed)
    | isInUrlImage dataParsing == True = do
        newDataParsed <- addCharToActualList ')' (dataParsing { isInUrlImage = False })
        (insertImageToParagraph newDataParsed)
    | otherwise = addBasicCharToActualList '[' dataParsing { isInContentLink = True }

parseOneChar '*' dataParsing = addCharToActualList '*' dataParsing -- For text formatting but maybe now unused

parseOneChar c dataParsing = addCharToActualList c dataParsing

------------------------------------------------------------------------------------------------------------
-----------------------------------               LINK                 -------------------------------------
------------------------------------------------------------------------------------------------------------

insertLinkToParagraph :: DataParsing -> IO DataParsing
insertLinkToParagraph dataParsing
    | length (actualList dataParsing) > 0 = do
        newDataParsed <- createText dataParsing
        (insertLink newDataParsed)
    | otherwise = (insertLink dataParsing)

insertLink :: DataParsing -> IO DataParsing
insertLink dataParsing = do
    newLink <- formattingLink dataParsing
    return (dataParsing { paragraph = (paragraph dataParsing) ++ [newLink], urlLink = "", contentLink = "" })

formattingLink :: DataParsing -> IO PParagraphType
formattingLink dataParsing = do
    textFormatted <- formattingText (contentLink dataParsing)
    return (PLinkParagraph (PLink (urlLink dataParsing) textFormatted))

------------------------------------------------------------------------------------------------------------
-----------------------------------             CREATE TEXT             ------------------------------------
------------------------------------------------------------------------------------------------------------

createText :: DataParsing -> IO DataParsing
createText dataParsing = do
    newTextType <- formattingElemParagraph dataParsing
    return dataParsing { paragraph = (paragraph dataParsing) ++ [newTextType] }

formattingElemParagraph :: DataParsing -> IO PParagraphType
formattingElemParagraph dataParsing = do
    textFormatted <- formattingText (actualList dataParsing)
    return (PTextParagraph textFormatted)

------------------------------------------------------------------------------------------------------------
-----------------------------------      FORMAT TEXT OF ACTUAL LIST     ------------------------------------
------------------------------------------------------------------------------------------------------------

formattingText :: String -> IO PText
formattingText str = do
    let dataText = initializeDataText    
    newDataText <- browseStr str dataText
    print newDataText
    return (PText [(PString (str))])

browseStr :: String -> DataText -> IO DataText
browseStr [] dataText = return dataText
browseStr (x:xs) dataText = do
    let (newStr, newDataText) = detectSymbol xs dataText

    if strcmp newStr xs
        then do
            let finalDataText = parseBasicChar newDataText x
            (browseStr xs finalDataText)
    else do
        browseStr xs newDataText

parseBasicChar :: DataText -> Char -> DataText
parseBasicChar dataText c = dataText { basicStr = (basicStr dataText) ++ [c] }

detectSymbol :: String -> DataText -> (String, DataText)
detectSymbol str dataText
    | Just (_, rightPart) <- isBold = 
        -- ! Check if isInBold == True -> Try the solution i think yesterday at night
        (rightPart, dataText { listText = listText dataText ++ [TBold Bold], isInBold = True })

    | Just (_, rightPart) <- isItalic =
        -- ! Check if isInItalic == True -> Try the solution i think yesterday at night
        (rightPart, dataText { listText = listText dataText ++ [TItalic Italic], isInItalic = True })

    | Just (_, rightPart) <- isCode =
        -- ! Check if isInCode == True -> Try the solution i think yesterday at night
        (rightPart, dataText { listText = listText dataText ++ [TCode Code], isInCode = True })

    | otherwise = (str, dataText)

    where
        isBold = parseString "**" str
        isItalic = parseString "*" str
        isCode = parseString "`" str

-- ** ` abc ` **
-- 
-- * PString [**] ` PString [abc **] ` *
-- 
-- InItalic
-- InBold
-- InCode
-- OutBold -- Here problem so -> when i arrive on a delimiter if his delimiter is Open don't take OutBold and Remove LastBold
-- OutCode
-- OutItalic
-- 
-- 
-- List of [InItalic, InBold, InCode, String]

-- if delimiter == True
-- 
-- Si les éléments du millieu ne sont pas refermés
-- 
-- List = [Bold, Code] -- Here want to delete last element who is normally a Bold But it's a Code so break the Bold




{--

-- ! Example

Bonjour **abc** !
PString "Bonjour ", PBold [PString "abc"], PString "!"

Bonjour **`abc`** !
PString "Bonjour ", PBold [PCode [PString "abc"]], PString "!"

Bonjour **`abc**` !
PString "Bonjour **", PCode [PString "abc**"], PString "!" 

--}

------------------------------------------------------------------------------------------------------------
-----------------------------------               IMAGE                 ------------------------------------
------------------------------------------------------------------------------------------------------------

insertImageToParagraph :: DataParsing -> IO DataParsing
insertImageToParagraph dataParsing
    | length (actualList dataParsing) > 0 = do
        newDataParsed <- createText dataParsing
        (insertImage newDataParsed)
    | otherwise = (insertImage dataParsing)

insertImage:: DataParsing -> IO DataParsing
insertImage dataParsing = do
    newImage <- formattingImg dataParsing
    return (dataParsing { paragraph = (paragraph dataParsing) ++ [newImage], urlImg = "", altImg = "" })

formattingImg :: DataParsing -> IO PParagraphType
formattingImg dataParsing = do
    textFormatted <- formattingText (altImg dataParsing)
    return (PImageParagraph (PImage (urlImg dataParsing) textFormatted))
