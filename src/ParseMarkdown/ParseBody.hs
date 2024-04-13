--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseBody
--

module ParseMarkdown.ParseBody (parseBody) where
import Content (PContent(..), PText(..), PParagraph(..), PParagraphType(..))
import ParsingLib.Lib (parseString)
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import ParseMarkdown.ParseOneChar (parseOneChar)
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)

parseBody :: DataParsing -> IO (Either String [PContent])
parseBody dataParsing =
    parseAllString (remainingLines dataParsing) dataParsing [] >>= \(allContent, dataParsed) ->
    print dataParsed >>
    print allContent >>
    return (Left "ok")

parseAllString :: [String] -> DataParsing -> [PContent] -> IO (Either String [PContent], DataParsing)
parseAllString [] dataParsing allContent = return (Right allContent, dataParsing)
parseAllString (x:xs) dataParsing allContent =
    parseEachString x x dataParsing False allContent >>= \(parsedContent, newDataParsing) ->
    return (newDataParsing { nbReturnLines = nbReturnLines dataParsing + 1 }, allContent ++ parsedContent) >>= \(newDataParsed, newContent) ->
    parseAllString xs newDataParsed newContent

-- parseAllString :: [String] -> DataParsing -> [PContent] -> IO (Either String [PContent], DataParsing)
-- parseAllString [] dataParsing allContent = return (Right allContent, dataParsing)
-- parseAllString (x:xs) dataParsing allContent =
--     parseEachString x x dataParsing False allContent >>= \stringParsed ->
--     (\(newContent, newDataParsed) ->
--         return (newDataParsed { nbReturnLines = nbReturnLines dataParsing + 1 }, newContent)
--     ) (tryAddElemToContent (Right stringParsed) allContent) >>= \(newDataParsed, newContent) ->
--     parseAllString xs newDataParsed newContent

------------------------------------------------------------------------------------------------------------
-----------------------------------       PARSE EACH STRING            -------------------------------------
------------------------------------------------------------------------------------------------------------

parseEachString :: String -> String -> DataParsing -> Bool -> [PContent] -> IO ([PContent], DataParsing)
parseEachString _ [] dataParsing _ allContent = return (allContent, dataParsing)
parseEachString str (c:cs) dataParsing hasCheckfirstStrIsAnElem allContent
    | not hasCheckfirstStrIsAnElem = checkFrstStr str dataParsing allContent
    | otherwise = analyseBasicString c cs dataParsing allContent

checkFrstStr :: String -> DataParsing -> [PContent] -> IO ([PContent] ,DataParsing)
checkFrstStr str dataParsing allContent =
    checkfirstStrIsAnElem str dataParsing >>= \(newDataParsing, newStr) ->
    parseEachString newStr newStr newDataParsing True allContent

analyseBasicString :: Char -> String -> DataParsing -> [PContent] -> IO ([PContent], DataParsing)
analyseBasicString c cs dataParsing allContent = do
    let (newContent, newDataParsed) = tryAddAnyElemToContent dataParsing allContent
    (dataAfterLinkImg, newStr) <- checkImgAndLink c cs newDataParsed
    case length newStr > 0 of
        True -> parseEachString newStr newStr dataAfterLinkImg True newContent
        False -> do
            newDataParsed' <- parseOneChar c newDataParsed
            parseEachString cs cs newDataParsed' True newContent

tryAddAnyElemToContent :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddAnyElemToContent dataParsing allContent
    | typeToAdd dataParsing == Link = (allContent, dataParsing)
    | typeToAdd dataParsing == Image = (allContent, dataParsing)
    | typeToAdd dataParsing == Paragraph = (allContent, dataParsing)  -- Check if in paragraph boolean
    | otherwise = (allContent, dataParsing)

-- tryAddElemToContent :: DataParsing -> [PContent] -> ([PContent], DataParsing)
-- tryAddElemToContent dataParsing allContent
--     | not (isInContentLink dataParsing) && not (isInAltImage dataParsing) && not (isInCodeblock dataParsing) && (nbReturnLines dataParsing) > 0 = addNewParagraph dataParsing allContent
--     | otherwise = (allContent, dataParsing)

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
    | Just (_, rightPart) <- isSectionSix = return (dataParsing { levelSection = 6 }, rightPart)
    | Just (_, rightPart) <- isSectionFive = return (dataParsing { levelSection = 5 }, rightPart)
    | Just (_, rightPart) <- isSectionFour = return (dataParsing { levelSection = 4 }, rightPart)
    | Just (_, rightPart) <- isSectionThree = return (dataParsing { levelSection = 3 }, rightPart)
    | Just (_, rightPart) <- isSectionTwo = return (dataParsing { levelSection = 2 }, rightPart)
    | Just (_, rightPart) <- isSectionOne = return (dataParsing { levelSection = 1 }, rightPart)
    | Just (_, rightPart) <- isCodeBlock = return (dataParsing { isInCodeblock = True }, rightPart)
    | Just (_, rightPart) <- isItem = return (dataParsing { levelItem = (levelItem dataParsing) + 1 }, rightPart)
    | otherwise = return (dataParsing, str) -- Call paragraph here after parse the string
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
-----------------------------------       CREATE ALL ELEMENTS          -------------------------------------
------------------------------------------------------------------------------------------------------------

-- Check if we are already in a paragraph
-- createParagraph :: DataParsing -> [PContent] -> IO ([PContent], DataParsing) 
-- createParagraph dataParsing allContent = return (allContent, dataParsing)
-- 
-- createItem :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
-- createItem dataParsing allContent = return (allContent, dataParsing)
-- 
-- createCodeBlock :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
-- createCodeBlock dataParsing allContent = return (allContent, dataParsing)
-- 
-- -- Check level Of Item if he is at 0 -> Create A list ELSE add Elem to List
-- createListOrAddItem :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
-- createListOrAddItem dataParsing allContent = return (allContent, dataParsing)
-- 
-- -- Compute with the level ->
-- createSecion :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
-- createSecion dataParsing allContent = return (allContent, dataParsing)

------------------------------------------------------------------------------------------------------------
-----------------------------------            PARAGRAPH               -------------------------------------
------------------------------------------------------------------------------------------------------------

-- addNewParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
-- addNewParagraph dataParsing allContent =
--     (addNewElemToContent actualContent allContent, parsedData)
--     where
--         actualContent = initializePParagraphContent dataParsing
--         parsedData = dataParsing {actualList = ""}
-- 
-- initializePText :: DataParsing -> PText
-- initializePText dataParsing = PText [Right (actualList dataParsing)]
-- 
-- initializePParagraphContent :: DataParsing -> PContent
-- initializePParagraphContent dataParsing =
--     PParagraphContent $ PParagraph [PTextParagraph $ initializePText dataParsing]

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
