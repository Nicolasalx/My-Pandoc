--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseOneChar
--

module ParseMarkdown.ParseOneChar (parseOneChar, createText) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeText(..), initializeDataText, DataText(..), ElemTextType(..))
import Content (PParagraphType(..), PText(..), PTextType(..), PLink(..), PImage(..), PBold(..), PItalic(..), PCode(..))
import ParsingLib.Lib (parseString)

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
    return (dataParsing { paragraph = (paragraph dataParsing) ++ [newLink], urlLink = "", contentLink = "", actualList = "" })

formattingLink :: DataParsing -> IO PParagraphType
formattingLink dataParsing = do
    textFormatted <- formattingText (contentLink dataParsing)
    return (PLinkParagraph (PLink (urlLink dataParsing) textFormatted))

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
    return (dataParsing { paragraph = (paragraph dataParsing) ++ [newImage], urlImg = "", altImg = "", actualList = "" })

formattingImg :: DataParsing -> IO PParagraphType
formattingImg dataParsing = do
    textFormatted <- formattingText (altImg dataParsing)
    return (PImageParagraph (PImage (urlImg dataParsing) textFormatted))

------------------------------------------------------------------------------------------------------------
-----------------------------------             CREATE TEXT             ------------------------------------
------------------------------------------------------------------------------------------------------------

createText :: DataParsing -> IO DataParsing
createText dataParsing
    | length (actualList dataParsing) > 0 = do
        newTextType <- formattingElemParagraph dataParsing
        return dataParsing { paragraph = (paragraph dataParsing) ++ [newTextType] }
    | otherwise = return dataParsing

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
    newData <- closeAllDelim finalData
    newList <- formatLastList dataText (listText newData) []
    -- Concat all strings in list
    endData <- appendAllElem newData newList
    return (contentText endData)

------------------------------------------------------------------------------------------------------------
-----------------------------------    APPEND ALL ELEM IN FINAL LIST    ------------------------------------
------------------------------------------------------------------------------------------------------------

appendAllElem :: DataText -> [ElemTextType] -> IO DataText
appendAllElem dataText [] = return dataText
appendAllElem dataText (x:xs)
    | x == (TBold Bold) && not (isInBold dataText) = do
        let newData = dataText { isInBold = True }
            newList = findGoodPosition (indexListText newData) (PBoldText (PBold [])) (contentText newData)
            finalData = newData { contentText = newList, indexListText = (indexListText newData) + 1 }
        appendAllElem finalData xs

    | x == (TBold Bold) && (isInBold dataText) = do
        let finalData = dataText { isInBold = False, indexListText = (indexListText dataText) - 1 }
        appendAllElem finalData xs
------------------------------------------------------------------------------------------
    | x == (TItalic Italic) && not (isInItalic dataText) = do
        let newData = dataText { isInItalic = True }
            newList = findGoodPosition (indexListText newData) (PItalicText (PItalic [])) (contentText newData)
            finalData = newData { contentText = newList, indexListText = (indexListText newData) + 1 }
        appendAllElem finalData xs

    | x == (TItalic Italic) && (isInItalic dataText) = do
        let finalData = dataText { isInItalic = False, indexListText = (indexListText dataText) - 1 }
        appendAllElem finalData xs
------------------------------------------------------------------------------------------
    | x == (TCode Code) && not (isInCode dataText) = do
        let newData = dataText { isInCode = True }
            newList = findGoodPosition (indexListText newData) (PCodeText (PCode [])) (contentText newData)
            finalData = newData { contentText = newList, indexListText = (indexListText newData) + 1 }
        appendAllElem finalData xs

    | x == (TCode Code) && (isInCode dataText) = do
        let finalData = dataText { isInCode = False, indexListText = (indexListText dataText) - 1 }
        appendAllElem finalData xs
------------------------------------------------------------------------------------------    
    | otherwise = case x of
        TString str -> do
            let newList = findGoodPosition (indexListText dataText) (PString str) (contentText dataText)
            appendAllElem (dataText { contentText = newList }) xs
        _ -> appendAllElem dataText xs

findGoodPosition :: Int -> PTextType -> PText -> PText
findGoodPosition index actualElem (PText list) = PText (insertAtIndex index actualElem (list))

insertAtIndex :: Int -> PTextType -> [PTextType] -> [PTextType]
insertAtIndex index actualElem list
    | index == 0 = list ++ [actualElem]
    | otherwise = go index list
    where
        go 0 rest = actualElem : rest
        go n (x:xs) = case x of
            PBoldText (PBold y) -> reverse (PBoldText (PBold (insertAtIndex (n - 1) actualElem y)) : xs)
            PItalicText (PItalic y) -> reverse (PItalicText (PItalic (insertAtIndex (n - 1) actualElem y)) : xs)
            PCodeText (PCode y) -> reverse (PCodeText (PCode (insertAtIndex (n - 1) actualElem y)) : xs)
            _ -> x : go n xs
        go _ [] = [actualElem]


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

formatLastList :: DataText -> [ElemTextType] -> [ElemTextType] -> IO [ElemTextType]
formatLastList _ [] finalList = return finalList
formatLastList dataText (x:xs) finalList
    | x == TBold Bold = do
        let (newData, newList) = setBoldList dataText x finalList
        formatLastList newData xs newList
    | x == TItalic Italic = do
        let (newData, newList) = setItalicList dataText x finalList
        formatLastList newData xs newList
    | x == TCode Code = do
        let (newData, newList) = setCodeList dataText x finalList
        formatLastList newData xs newList
    | otherwise = formatLastList dataText xs (finalList ++ [x])

removeElement :: ElemTextType -> [ElemTextType] -> [ElemTextType] -> [ElemTextType]
removeElement _ [] newList = newList
removeElement elemText (x:xs) newList
    | x == elemText && elemText == (TBold Bold) = removeElement elemText xs (newList ++ [(TString "**")])
    | x == elemText && elemText == (TItalic Italic) = removeElement elemText xs (newList ++ [(TString "*")])
    | otherwise = removeElement elemText xs (newList ++ [x])

setBoldList :: DataText -> ElemTextType -> [ElemTextType] -> (DataText, [ElemTextType])
setBoldList dataText actualElem actualListText
    | (isInCode dataText) && not (isInBold dataText) = do
        -- ! To finish
        -- reverse List
        let listWithoutFront = removeElement actualElem actualListText []
        -- reverse at normal
        (dataText { isInBold = True }, listWithoutFront)
    | (isInCode dataText) && (isInBold dataText) = do
        -- ! To finish
        let listWithoutBack = removeElement actualElem actualListText []  
        (dataText { isInBold = False }, listWithoutBack)
    | not (isInBold dataText) = (dataText { isInBold = True } , actualListText ++ [actualElem])
    | isInBold dataText = (dataText { isInBold = False } , actualListText ++ [actualElem])
    | otherwise = (dataText, actualListText)

setItalicList :: DataText -> ElemTextType -> [ElemTextType] -> (DataText, [ElemTextType])
setItalicList dataText actualElem actualListText
    | (isInCode dataText) && not (isInItalic dataText) = do
        -- ! To finish
        -- reverse at list
        let listWithoutFront = removeElement actualElem actualListText []
        -- reverse at normal
        (dataText { isInItalic = True }, listWithoutFront)
    | (isInCode dataText) && (isInItalic dataText) = do
        -- ! To finish
        let listWithoutBack = removeElement actualElem actualListText []
        (dataText { isInItalic = False }, listWithoutBack)
    | not (isInItalic dataText) = (dataText { isInItalic = True } , actualListText ++ [actualElem])
    | isInItalic dataText = (dataText { isInItalic = False } , actualListText ++ [actualElem])
    | otherwise = (dataText, actualListText)

setCodeList :: DataText -> ElemTextType -> [ElemTextType] -> (DataText, [ElemTextType])
setCodeList dataText actualElem actualListText
    | not (isInCode dataText) = (dataText { isInCode = True } , actualListText ++ [actualElem])
    | otherwise = (dataText { isInCode = False } , actualListText ++ [actualElem])

------------------------------------------------------------------------------------------------------------
-----------------------------------             BROWSE STR              ------------------------------------
------------------------------------------------------------------------------------------------------------

browseStr :: String -> DataText -> Bool -> IO DataText
browseStr [] dataText _ = return dataText
browseStr (x:xs) dataText hasTakeFrstChar
    | not hasTakeFrstChar = do
        let newDataText = dataText { precedentChar = x }
        (newStr, finalDataText) <- detectSymbol ([x] ++ xs) newDataText
        browseStr newStr finalDataText True
    | otherwise = do
        let newDataText = dataText { precedentChar = x }
        (newStr, finalDataText) <- detectSymbol xs newDataText
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

symbolBoldAlreadyOpen :: String -> DataText -> IO (String, DataText)
symbolBoldAlreadyOpen str dataText = do
    if ((length (basicStr dataText) == 0  || ((precedentChar dataText) /= ' ') && (last (basicStr dataText) /= ' ')))
        then do
            let tmpDataText = tryAddBasicToList dataText
                newData = tmpDataText { isInBold = False, listText = (listText tmpDataText) ++ [TBold Bold] }
            return ([' '] ++ str, newData)
    else do
        let newData = fillInvalidPattern 0 2 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return ([' '] ++ str, newData)
        else
            return ([' '] ++ str, newData)

symbolBoldNotOpen :: String -> DataText -> IO (String, DataText)
symbolBoldNotOpen str dataText = do
    if ((length (str) > 0 && head (str) /= ' '))
        then do
            let endData = tryAddBasicToList dataText
                newData = endData { isInBold = True, listText = (listText endData) ++ [TBold Bold] }
            return ([' '] ++str, newData)
    else do
        let newData = fillInvalidPattern 0 2 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return (str, newData)
        else
            return ([' '] ++str, newData)

------------------------------------------------------------------------------------------------------------
-----------------------------------               ITALIC                ------------------------------------
------------------------------------------------------------------------------------------------------------

symbolItalicAlreadyOpen :: String -> DataText -> IO (String, DataText)
symbolItalicAlreadyOpen str dataText = do
    if ((length (basicStr dataText) == 0  || (precedentChar dataText) /= ' ' && (last (basicStr dataText) /= ' ')))
        then do
            let tmpDataText = tryAddBasicToList dataText
                newData = tmpDataText { isInItalic = False, listText = (listText tmpDataText) ++ [TItalic Italic] }
            return ([' '] ++str, newData)
    else do
        let newData = fillInvalidPattern 0 1 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return ([' '] ++ str, newData)
        else
            return ([' '] ++ str, newData)

symbolItalicNotOpen :: String -> DataText -> IO (String, DataText)
symbolItalicNotOpen str dataText = do
    if ((length (str) > 0 && head (str) /= ' '))
        then do
            let endData = tryAddBasicToList dataText
                newData = endData { isInItalic = True, listText = (listText endData) ++ [TItalic Italic] }

            return ([' '] ++ str, newData)
    else do
        let newData = fillInvalidPattern 0 1 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return ([' '] ++ str, newData)
        else
            return ([' '] ++ str, newData)

------------------------------------------------------------------------------------------------------------
-----------------------------------               CODE                  ------------------------------------
------------------------------------------------------------------------------------------------------------

symbolCodedAlreadyOpen :: String -> DataText -> IO (String, DataText)
symbolCodedAlreadyOpen str dataText = do
    let tmpDataText = tryAddBasicToList dataText
        newData = tmpDataText { isInCode = False, listText = (listText tmpDataText) ++ [TCode Code] }
    return ([' '] ++ str, newData)

symbolCodeNotOpen :: String -> DataText -> IO (String, DataText)
symbolCodeNotOpen str dataText = do
    let endData = tryAddBasicToList dataText
        newData = endData { isInCode = True, listText = (listText endData) ++ [TCode Code] }
    return ([' '] ++ str, newData)

------------------------------------------------------------------------------------------------------------
-----------------------------------            DETECT SYMBOL            ------------------------------------
------------------------------------------------------------------------------------------------------------

detectSymbol :: String -> DataText -> IO (String, DataText)
detectSymbol str dataText
    | Just (_, rightPart) <- isBold, not (isInItalic dataText) = do
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
    | Just (_, rightPart) <- isBold = do
        if isInBold dataText
            then
                (symbolBoldAlreadyOpen rightPart dataText) 
        else
            (symbolBoldNotOpen rightPart dataText)

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
                return (str, newDataText)
        else
            return (str, dataText)            
    where
        isBold = parseString "**" str
        isItalic = parseString "*" str
        isCode = parseString "`" str

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
