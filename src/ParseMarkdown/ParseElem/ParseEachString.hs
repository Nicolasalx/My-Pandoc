--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseEachString
--

module ParseMarkdown.ParseElem.ParseEachString (parseEachString) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..))
import ParsingLib.Lib (strcmp)
import ParseMarkdown.ParseElem.CheckImgAndLinks (checkImgAndLink)
import ParseMarkdown.ParseOneChar (parseOneChar)
import ParsingLib.Lib (parseString)
import ParseMarkdown.ParseElem.Paragraph (tryAddParagraph)
import ParseMarkdown.ParseElem.Section (createSection)
import ParseMarkdown.ParseElem.Codeblock (tryAddCodeBlock, parseStartCodeBlock)
import ParseMarkdown.ParseElem.ParagraphType (defineParagraphType)
import ParseMarkdown.ParseElem.Item (createItem)
import ParseMarkdown.ParseElem.SkipSpaces (skipSpaces)

parseEachString :: String -> String -> DataParsing -> Bool -> [PContent] -> ([PContent], DataParsing)
parseEachString _ [] dataParsing _ allContent = (allContent, dataParsing)
parseEachString str (c:cs) dataParsing hasCheckfirstStrIsAnElem allContent
    | not hasCheckfirstStrIsAnElem = checkFrstStr str dataParsing allContent
    | otherwise = analyseBasicString c cs dataParsing allContent

analyseBasicString :: Char -> String -> DataParsing -> [PContent] -> ([PContent], DataParsing)
analyseBasicString c cs dataParsing allContent = do 
    let (dataAfterLinkImg, newStr) = checkImgAndLink c cs dataParsing
    case strcmp newStr cs of
        True -> do
            let newDataParsed' = parseOneChar c dataParsing
            parseEachString cs cs newDataParsed' True allContent
        False ->
            parseEachString newStr newStr dataAfterLinkImg True allContent

checkFrstStr :: String -> DataParsing -> [PContent] -> ([PContent] ,DataParsing)
checkFrstStr str dataParsing allContent = do
    let (newDataParsing, newStr, newContent) = checkfirstStrIsAnElem str dataParsing allContent
    if strcmp str newStr
        then
        parseEachString newStr newStr newDataParsing True newContent
    else
        parseEachString "" "" newDataParsing True newContent

checkfirstStrIsAnElem :: String -> DataParsing -> [PContent] -> (DataParsing, String, [PContent])
checkfirstStrIsAnElem str dataParsing allContent
    | Just (_, rightPart) <- isSectionSix = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
            (finalContent, finalData) = createSection 6 rightPart newData newContent
        (finalData { levelSection = 6, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionFive = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
            (finalContent, finalData) = createSection 5 rightPart newData newContent
        (finalData { levelSection = 5, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionFour = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
            (finalContent, finalData) = createSection 4 rightPart newData newContent
        (finalData { levelSection = 4, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionThree = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
            (finalContent, finalData) = createSection 3 rightPart newData newContent
        (finalData { levelSection = 3, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionTwo = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
            (finalContent, finalData) = createSection 2 rightPart newData newContent
        (finalData { levelSection = 2, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionOne = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
            (finalContent, finalData) = createSection 1 rightPart newData newContent
        (finalData { levelSection = 1, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isCodeBlock = do
        let (newfrstContent, newData) = tryAddParagraph dataParsing allContent
            newDataParsed = newData { isInCodeblock = not (isInCodeblock dataParsing), typeToAdd = CodeBlock }
            (finalDataParsed, newContent) = tryAddCodeBlock newDataParsed newfrstContent
        (finalDataParsed, rightPart, newContent)
    | Just (_, rightPart) <- isItem = do
        -- ! Item
        -- (newContent, newData) <- tryAddParagraph dataParsing allContent
        -- (finalContent, finalData) <- createItem str rightPart newData newContent
-- 
        -- defineParagraphType dataParsing str allContent

        (dataParsing { levelItem = (levelItem dataParsing) + 1, typeToAdd = Item }, rightPart, allContent)

    | otherwise = defineParagraphType dataParsing str allContent
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
