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
parseOneChar '[' dataParsing = addBasicCharToActualList '[' dataParsing { isInContentLink = True }

parseOneChar ')' dataParsing
    | isInUrlLink dataParsing == True = do
        newDataParsed <- addCharToActualList ')' (dataParsing { isInUrlLink = False })
        (insertLinkToParagraph newDataParsed)
    | isInUrlImage dataParsing == True = do
        newDataParsed <- addCharToActualList ')' (dataParsing { isInUrlImage = False })
        (insertImageToParagraph newDataParsed)
    | otherwise = addBasicCharToActualList '[' dataParsing { isInContentLink = True }

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
    newDataText <- browseStr str dataText False
    let finalData = tryAddBasicToList newDataText
    print (listText finalData)
    newData <- closeAllDelim finalData
    print ("New data: " ++ show (listText newData))

    return (PText [(PString (str))])

------------------------------------------------------------------------------------------------------------
-----------------------------------          REMOVE UNUSED DELIM        ------------------------------------
------------------------------------------------------------------------------------------------------------

closeAllDelim :: DataText -> IO DataText
closeAllDelim dataText = do
    dataBold <- checkRemoveBold dataText
    dataItalic <- checkRemoveItalic dataBold
    dataCode <- checkRemoveCode dataItalic
    return dataCode

checkRemoveBold :: DataText -> IO DataText
checkRemoveBold dataText
    | isInBold dataText = do
        updatedList <- (removeLastDelim (TBold Bold) (listText dataText) [] "**")
        return dataText { listText = updatedList }
    | otherwise = return dataText

checkRemoveItalic :: DataText -> IO DataText
checkRemoveItalic dataText
    | isInItalic dataText = do
        updatedList <- (removeLastDelim (TItalic Italic) (listText dataText) [] "*")
        return dataText { listText = updatedList }
    | otherwise = return dataText

checkRemoveCode :: DataText -> IO DataText
checkRemoveCode dataText
    | isInCode dataText = do
        updatedList <- (removeLastDelim (TCode Code) (listText dataText) [] "`")
        return dataText { listText = updatedList }
    | otherwise = return dataText

removeLastDelim :: ElemTextType -> [ElemTextType] -> [ElemTextType] -> String -> IO [ElemTextType]
removeLastDelim delimToDelete basicList finalList elemToReplace = replaceLastDelim delimToDelete basicList finalList elemToReplace

replaceLastDelim :: ElemTextType -> [ElemTextType] -> [ElemTextType] -> String -> IO [ElemTextType]
replaceLastDelim _ [] finalList _ = return finalList
replaceLastDelim delimToDelete (x:xs) finalList elemToReplace
    | x == delimToDelete = replaceLastDelim delimToDelete xs (finalList ++ [TString elemToReplace]) elemToReplace 
    | otherwise = replaceLastDelim delimToDelete xs (finalList ++ [x]) elemToReplace 

------------------------------------------------------------------------------------------------------------
-----------------------------------       FORMATTING FINAL DATA LIST    ------------------------------------
------------------------------------------------------------------------------------------------------------

-- formatLastList ::
-- 
-- 
-- -- Si code set 
-- 
-- - Si on rentre dans un code et qu'on voit un délimiteur pas code:
-- - On remplace le délimiteur par ses charactères et on remplace aussi celui qui le ferme
-- Exemple : abc `bold **and code`
-- -
-- Exempl : abc ** * def * **

-- TODO LIST
{-
    - Gérer ce cas : ***a*** => actuellement ca fait Bold, Italic, String, Bold, Italic
    (Trouver un moyen de checker Italic avant Bold dans checkSymbol si le Bollean isInItalic est à true)


-}

------------------------------------------------------------------------------------------------------------
-----------------------------------             BROWSE STR              ------------------------------------
------------------------------------------------------------------------------------------------------------

browseStr :: String -> DataText -> Bool -> IO DataText
browseStr [] dataText _ = return dataText
browseStr (x:xs) dataText hasTakeFrstChar
    | not hasTakeFrstChar = do
        let newDataText = dataText { precedentChar = x }
        (newStr, finalDataText, isValidPattern) <- detectSymbol ([x] ++ xs) newDataText
        browseStr newStr finalDataText True
    | otherwise = do
        let newDataText = dataText { precedentChar = x }
        (newStr, finalDataText, isValidPattern) <- detectSymbol xs newDataText
        browseStr newStr finalDataText True

parseBasicChar :: DataText -> Char -> DataText
parseBasicChar dataText c = dataText { basicStr = (basicStr dataText) ++ [c] }

fillInvalidPattern :: Int -> Int -> Char -> DataText -> DataText
fillInvalidPattern index limit c dataText
    | index == limit = dataText
    | otherwise = do
        let newDataText = parseBasicChar dataText c
            endData = newDataText { precedentChar = c }
        fillInvalidPattern (index + 1) limit c endData

tryAddBasicToList :: DataText -> DataText
tryAddBasicToList dataText
    | length (basicStr dataText) > 0 = dataText { listText = (listText dataText) ++ [(TString (basicStr dataText))], basicStr = "" }
    | otherwise = dataText

------------------------------------------------------------------------------------------------------------
-----------------------------------               BOLD                  ------------------------------------
------------------------------------------------------------------------------------------------------------

symbolBoldAlreadyOpen :: String -> DataText -> IO (String, DataText, Bool)
symbolBoldAlreadyOpen str dataText = do
    if ((length (basicStr dataText) == 0  || ((precedentChar dataText) /= ' ') && (last (basicStr dataText) /= ' ')))
        then do
            let tmpDataText = tryAddBasicToList dataText
                newData = tmpDataText { isInBold = False, listText = (listText tmpDataText) ++ [TBold Bold] }
            return ([' '] ++ str, newData, True)
    else do
        let newData = fillInvalidPattern 0 2 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return ([' '] ++ str, newData, True)
        else
            return ([' '] ++ str, newData, True)

symbolBoldNotOpen :: String -> DataText -> IO (String, DataText, Bool)
symbolBoldNotOpen str dataText = do
    if ((length (str) > 0 && head (str) /= ' '))
        then do
            let endData = tryAddBasicToList dataText
                newData = endData { isInBold = True, listText = (listText endData) ++ [TBold Bold] }
            return ([' '] ++str, newData, True)
    else do
        let newData = fillInvalidPattern 0 2 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return (str, newData, True)
        else
            return ([' '] ++str, newData, True)

------------------------------------------------------------------------------------------------------------
-----------------------------------               ITALIC                ------------------------------------
------------------------------------------------------------------------------------------------------------

symbolItalicAlreadyOpen :: String -> DataText -> IO (String, DataText, Bool)
symbolItalicAlreadyOpen str dataText = do
    if ((length (basicStr dataText) == 0  || (precedentChar dataText) /= ' ' && (last (basicStr dataText) /= ' ')))
        then do
            let tmpDataText = tryAddBasicToList dataText
                newData = tmpDataText { isInItalic = False, listText = (listText tmpDataText) ++ [TItalic Italic] }
            return ([' '] ++str, newData, True)
    else do
        let newData = fillInvalidPattern 0 1 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return ([' '] ++ str, newData, True)
        else
            return ([' '] ++ str, newData, True)

symbolItalicNotOpen :: String -> DataText -> IO (String, DataText, Bool)
symbolItalicNotOpen str dataText = do
    if ((length (str) > 0 && head (str) /= ' '))
        then do
            let endData = tryAddBasicToList dataText
                newData = endData { isInItalic = True, listText = (listText endData) ++ [TItalic Italic] }

            return ([' '] ++ str, newData, True)
    else do
        let newData = fillInvalidPattern 0 1 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return ([' '] ++ str, newData, True)
        else
            return ([' '] ++ str, newData, True)

------------------------------------------------------------------------------------------------------------
-----------------------------------               CODE                  ------------------------------------
------------------------------------------------------------------------------------------------------------

symbolCodedAlreadyOpen :: String -> DataText -> IO (String, DataText, Bool)
symbolCodedAlreadyOpen str dataText = do
    let tmpDataText = tryAddBasicToList dataText
        newData = tmpDataText { isInCode = False, listText = (listText tmpDataText) ++ [TCode Code] }
    return ([' '] ++ str, newData, True)

symbolCodeNotOpen :: String -> DataText -> IO (String, DataText, Bool)
symbolCodeNotOpen str dataText = do
    let endData = tryAddBasicToList dataText
        newData = endData { isInCode = True, listText = (listText endData) ++ [TCode Code] }
    return ([' '] ++ str, newData, True)

------------------------------------------------------------------------------------------------------------
-----------------------------------            DETECT SYMBOL            ------------------------------------
------------------------------------------------------------------------------------------------------------

detectSymbol :: String -> DataText -> IO (String, DataText, Bool)
detectSymbol str dataText
    | Just (_, rightPart) <- isBold = do
        if isInBold dataText
            then
                (symbolBoldAlreadyOpen rightPart dataText) 
        else
            (symbolBoldNotOpen rightPart dataText)

    | Just (_, rightPart) <- isItalic = do
        if isInItalic dataText
            then
                (symbolItalicAlreadyOpen rightPart dataText) 
        else
            (symbolItalicNotOpen rightPart dataText)

    | Just (_, rightPart) <- isCode = do
        if isInCode dataText
            then
                (symbolCodedAlreadyOpen rightPart dataText) 
        else
            (symbolCodeNotOpen rightPart dataText)

    | otherwise = do
        if (length str > 0)
            then do
                let newDataText = parseBasicChar dataText (head str)
                return (str, newDataText, False)
        else
            return (str, dataText, False)            
    where
        isBold = parseString "**" str
        isItalic = parseString "*" str
        isCode = parseString "`" str

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

------------------------------------------------------------------------------------------------------------
-----------------------------------           CHECK SYMBOL              ------------------------------------
------------------------------------------------------------------------------------------------------------

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
