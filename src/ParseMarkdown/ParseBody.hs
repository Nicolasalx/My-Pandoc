--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseBody
--

module ParseMarkdown.ParseBody (parseBody) where
import Content (PContent(..), PText(..), PParagraph(..), PParagraphType(..))
import ParsingLib.Lib (parseString)
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import ParseMarkdown.ParseOneChar (parseOneChar)
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)
import Control.Monad (guard)

parseBody :: DataParsing -> IO (Either String [PContent])
parseBody dataParsing = do
    let allContent = []
    (allContent, dataParsed) <- parseAllString (remainingLines dataParsing) dataParsing allContent
    print dataParsed
    print allContent
    return (Left "ok")

parseAllString :: [String] -> DataParsing -> [PContent] -> IO (Either String [PContent], DataParsing)
parseAllString [] dataParsing allContent = return (Right allContent, dataParsing)
parseAllString (x:xs) dataParsing allContent =
    parseEachString x x dataParsing False >>= \stringParsed ->
    (\(newContent, newDataParsed) ->
        return (newDataParsed { nbReturnLines = nbReturnLines dataParsing + 1 }, newContent)
    ) (tryAddElemToContent stringParsed allContent) >>= \(newDataParsed, newContent) ->
    parseAllString xs newDataParsed newContent

------------------------------------------------------------------------------------------------------------
-----------------------------------       PARSE EACH STRING            -------------------------------------
------------------------------------------------------------------------------------------------------------

parseEachString :: String -> String -> DataParsing -> Bool -> IO DataParsing
parseEachString _ [] dataParsing _ = return dataParsing
parseEachString str (c:cs) dataParsing hasCheckfirstStrIsAnElem
    | not hasCheckfirstStrIsAnElem = checkFrstStr str dataParsing
    | otherwise = analyseBasicString c cs dataParsing

checkFrstStr :: String -> DataParsing -> IO DataParsing
checkFrstStr str dataParsing =
    checkfirstStrIsAnElem str dataParsing >>= \(newDataParsing, newStr) ->
    parseEachString newStr newStr newDataParsing True

analyseBasicString :: Char -> String -> DataParsing -> IO DataParsing
analyseBasicString c cs dataParsing = do
    (dataAfterLinkImg, newStr) <- checkImgAndLink c cs dataParsing
    if ((length newStr) > 0)
    then do
        parseEachString newStr newStr dataAfterLinkImg True
    else do
        newDataParsed <- parseOneChar c dataParsing
        parseEachString cs cs newDataParsed True

tryAddElemToContent :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddElemToContent dataParsing allContent
    | not (isInContentLink dataParsing) && not (isInAltImage dataParsing) && not (isInCodeblock dataParsing) && (nbReturnLines dataParsing) > 0 = addNewParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

------------------------------------------------------------------------------------------------------------
-----------------------------------       CHECK IMG AND LINK           -------------------------------------
------------------------------------------------------------------------------------------------------------

checkImgAndLink :: Char -> String -> DataParsing -> IO (DataParsing, String)
checkImgAndLink c str dataParsing
    | Just (_, rightPart) <- parseString "![" str =
        return (dataParsing { isInAltImage = True }, rightPart)
    | Just (_, rightPart) <- parseString "](" str =
        endLinkOrImg c rightPart dataParsing
    | otherwise = return (dataParsing, "")

endLinkOrImg :: Char -> String -> DataParsing -> IO (DataParsing, String)
endLinkOrImg c str dataParsing
    | isInContentLink dataParsing == True =
        parseOneChar c dataParsing >>= \newDataParsed ->
        return (newDataParsed { isInContentLink = False, isInUrlLink = True }, str)
    | isInAltImage dataParsing == True =
        parseOneChar c dataParsing >>= \newDataParsed ->
        return (newDataParsed { isInAltImage = False, isInUrlImage = True }, str)
    | otherwise = return (dataParsing, str)

------------------------------------------------------------------------------------------------------------
-----------------------------------          CHECK ALL ELEM            -------------------------------------
------------------------------------------------------------------------------------------------------------

skipSpaces :: Int -> String -> String
skipSpaces _ [] = []
skipSpaces 0 str = str
skipSpaces index (x:xs)
    | x == ' ' = skipSpaces (index-1) xs
    | otherwise = (x:xs)

checkfirstStrIsAnElem :: String -> DataParsing -> IO (DataParsing, String)
checkfirstStrIsAnElem str dataParsing
    | Just (_, rightPart) <- isSectionSix = return (dataParsing, rightPart)
    | Just (_, rightPart) <- isSectionFive = return (dataParsing, rightPart)
    | Just (_, rightPart) <- isSectionFour = return (dataParsing, rightPart)
    | Just (_, rightPart) <- isSectionThree = return (dataParsing, rightPart)
    | Just (_, rightPart) <- isSectionTwo = return (dataParsing, rightPart)
    | Just (_, rightPart) <- isSectionOne = return (dataParsing, rightPart)
    | Just (_, rightPart) <- isCodeBlock = return (dataParsing, rightPart)
    | Just (_, rightPart) <- isItem = return (dataParsing, rightPart)
    | otherwise = return (dataParsing, str)
  where
    stringSkipSpaces = skipSpaces 3 str
    isSectionSix = parseString "###### " stringSkipSpaces
    isSectionFive = parseString "##### " stringSkipSpaces
    isSectionFour = parseString "#### " stringSkipSpaces
    isSectionThree = parseString "### " stringSkipSpaces
    isSectionTwo = parseString "## " stringSkipSpaces
    isSectionOne = parseString "# " stringSkipSpaces
    isCodeBlock = parseString "``` " stringSkipSpaces
    isItem = parseString "-" stringSkipSpaces

------------------------------------------------------------------------------------------------------------
-----------------------------------            PARAGRAPH               -------------------------------------
------------------------------------------------------------------------------------------------------------

addNewParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
addNewParagraph dataParsing allContent =
    (addNewElemToContent actualContent allContent, parsedData)
    where
        actualContent = initializePParagraphContent dataParsing
        parsedData = dataParsing {actualList = ""}

initializePText :: DataParsing -> PText
initializePText dataParsing = PText [Right (actualList dataParsing)]

initializePParagraphContent :: DataParsing -> PContent
initializePParagraphContent dataParsing =
    PParagraphContent $ PParagraph [PTextParagraph $ initializePText dataParsing]

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
