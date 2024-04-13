--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseBody
--

module ParseMarkdown.ParseBody (parseBody) where
import Content (PContent(..), PText(..), PParagraph(..), PParagraphType(..))
import ParsingLib.Lib (parseString, Parser, strcmp)
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
parseAllString (x:xs) dataParsing allContent = do
    (content, stringParsed) <- parseEachString x x dataParsing False allContent
    (newContent, newData) <- tryAddElemToContent stringParsed content
    let newDataParsed = newData { nbReturnLines = nbReturnLines dataParsing + 1 }
        finalData = fillElemEmptyActualList newDataParsed
    parseAllString xs finalData newContent

-- A paragraph is cut by 2 \n too (i don't check actually)

tryAddElemToContent :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
tryAddElemToContent dataParsing allContent
    | typeToAdd dataParsing == Paragraph = return (createParagraph dataParsing allContent)
    | typeToAdd dataParsing == Item = return (createItem dataParsing allContent)
    | typeToAdd dataParsing == Section = return (createSection dataParsing allContent)
    | otherwise = return (allContent, dataParsing)

-- ! This function will make action with actualList every \n

fillElemEmptyActualList :: DataParsing -> DataParsing
fillElemEmptyActualList dataParsing
    | isInCodeblock dataParsing == True = dataParsing { actualCodeBloc = actualCodeBloc dataParsing ++ [(actualList dataParsing)], actualList = "" }
    -- Implement new line for item
    | otherwise = dataParsing

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
    checkfirstStrIsAnElem str dataParsing allContent >>= \(newDataParsing, newStr, newContent) ->
    parseEachString newStr newStr newDataParsing True newContent

analyseBasicString :: Char -> String -> DataParsing -> [PContent] -> IO ([PContent], DataParsing)
analyseBasicString c cs dataParsing allContent = do
    (dataAfterLinkImg, newStr) <- checkImgAndLink c cs dataParsing
    case strcmp newStr cs of
        True -> do
            newDataParsed' <- parseOneChar c dataParsing
            parseEachString cs cs newDataParsed' True allContent 
        False ->
            parseEachString newStr newStr dataAfterLinkImg True allContent

------------------------------------------------------------------------------------------------------------
-----------------------------------       CHECK IMG AND LINK           -------------------------------------
------------------------------------------------------------------------------------------------------------

checkImgAndLink :: Char -> String -> DataParsing -> IO (DataParsing, String)
checkImgAndLink c str dataParsing
    | Just (_, rightPart) <- parseString "![" str =
        return (dataParsing { isInAltImage = True }, rightPart)
    | Just (_, rightPart) <- parseString "](" str =
        endLinkOrImg c rightPart dataParsing
    | otherwise = return (dataParsing, str)

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

checkfirstStrIsAnElem :: String -> DataParsing -> [PContent] -> IO (DataParsing, String, [PContent])
checkfirstStrIsAnElem str dataParsing allContent
    | Just (_, rightPart) <- isSectionSix = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        return (newData { levelSection = 6, typeToAdd = Section }, rightPart, newContent)
    | Just (_, rightPart) <- isSectionFive = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        return (newData { levelSection = 5, typeToAdd = Section }, rightPart, newContent)
    | Just (_, rightPart) <- isSectionFour = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        return (newData { levelSection = 4, typeToAdd = Section }, rightPart, newContent)
    | Just (_, rightPart) <- isSectionThree = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        return (newData { levelSection = 3, typeToAdd = Section }, rightPart, newContent)
    | Just (_, rightPart) <- isSectionTwo = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        return (newData { levelSection = 2, typeToAdd = Section }, rightPart, newContent)
    | Just (_, rightPart) <- isSectionOne = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        return (newData { levelSection = 1, typeToAdd = Section }, rightPart, newContent)
    | Just (_, rightPart) <- isCodeBlock = do
        let (newfrstContent, newData) = tryAddParagraph dataParsing allContent
        let newDataParsed = newData { isInCodeblock = not (isInCodeblock dataParsing), typeToAdd = CodeBlock }
            newContent = tryAddCodeBlock dataParsing newfrstContent
        return (newDataParsed, rightPart, newContent)
    | Just (_, rightPart) <- isItem = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        return (newData { levelItem = (levelItem dataParsing) + 1, typeToAdd = Item }, rightPart, newContent)
    | otherwise = defineParagraphType dataParsing str allContent -- Call paragraph here after parse the string
  where
    stringSkipSpaces = skipSpaces 3 str
    isSectionSix = parseString "###### " stringSkipSpaces
    isSectionFive = parseString "##### " stringSkipSpaces
    isSectionFour = parseString "#### " stringSkipSpaces
    isSectionThree = parseString "### " stringSkipSpaces
    isSectionTwo = parseString "## " stringSkipSpaces
    isSectionOne = parseString "# " stringSkipSpaces
    isCodeBlock = parseStartCodeBlock str
    isItem = parseString "-" stringSkipSpaces

defineParagraphType :: DataParsing -> String -> [PContent] -> IO (DataParsing, String, [PContent])
defineParagraphType dataParsing str allContent
    | not (isInCodeblock dataParsing) && not (isInParagraph dataParsing) = return (dataParsing { typeToAdd = Paragraph }, str, allContent)
    | otherwise = return (dataParsing, str, allContent)

------------------------------------------------------------------------------------------------------------
-----------------------------------            PARAGRAPH               -------------------------------------
------------------------------------------------------------------------------------------------------------

createParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
createParagraph dataParsing allContent =
    (addNewElemToContent actualContent allContent, parsedData)
    where
        actualContent = initializePParagraphContent dataParsing
        parsedData = dataParsing {actualList = ""}
        -- After create the Paragraph, i will place it in the correct section

initializePText :: DataParsing -> PText
initializePText dataParsing = PText [Right (actualList dataParsing)]

initializePParagraphContent :: DataParsing -> PContent
initializePParagraphContent dataParsing =
    PParagraphContent $ PParagraph [PTextParagraph $ initializePText dataParsing]

tryAddParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddParagraph dataParsing allContent
    | typeToAdd dataParsing == Paragraph = createParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

------------------------------------------------------------------------------------------------------------
-----------------------------------            ITEM                    -------------------------------------
------------------------------------------------------------------------------------------------------------

-- Check actual Level Item
createItem :: DataParsing -> [PContent] -> ([PContent], DataParsing)
createItem dataParsing allContent = (allContent, dataParsing)
-- After create the Item, i will place it in the correct section

------------------------------------------------------------------------------------------------------------
-----------------------------------          CODEBLOCK                 -------------------------------------
------------------------------------------------------------------------------------------------------------

startsWithThreeBackticksAndSpaces :: String -> Bool
startsWithThreeBackticksAndSpaces "```" = True
startsWithThreeBackticksAndSpaces ('`':'`':'`':rest) = all (== ' ') rest
startsWithThreeBackticksAndSpaces _ = False

parseStartCodeBlock :: String -> Maybe (String, String)
parseStartCodeBlock str =
    if startsWithThreeBackticksAndSpaces str
        then
        parseString "```" str
    else
        Nothing

tryAddCodeBlock :: DataParsing -> [PContent] -> [PContent]
tryAddCodeBlock dataParsing allContent = do
    if isInCodeblock dataParsing
        then
            -- Finish to format the codeBlock
            -- Add A new Block at the good emplacement
            allContent
    else
        allContent

-- ! To finish
-- addCodeBlockToContent :: DataParsing -> PCodeBlock -> [PContent]
-- addCodeBlockToContent dataParsing codeBlock =


------------------------------------------------------------------------------------------------------------
-----------------------------------          SECTION                   -------------------------------------
------------------------------------------------------------------------------------------------------------

createSection :: DataParsing -> [PContent] -> ([PContent], DataParsing)
createSection dataParsing allContent = (allContent, dataParsing)

------------------------------------------------------------------------------------------------------------
-----------------------------------      INSERT AT GOOD PLACE          -------------------------------------
------------------------------------------------------------------------------------------------------------

{-
This function will take a Content and it will be place into [PContent]

If the Content is a Paragraph OR a CodeBlock
    if we are in a section with levelSection:

    else:
        allContent ++ actualContent

-- Compute with the levelSection

-}


-- ! TO DO LIST
-- Modify the function for insert in the PContent for all the insert
-- allCodeBlcok [] is actually perfectly fill -> Now just insert at the good place and CodeBlock are finished
