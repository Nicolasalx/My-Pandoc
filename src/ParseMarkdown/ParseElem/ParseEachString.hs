{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseEachString
-}

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
import ParseMarkdown.ParseElem.Item (tryAddItem)


parseEachString :: String -> String -> DataParsing ->
    Bool -> [PContent] -> ([PContent], DataParsing)
parseEachString _ [] dataParsing _ allContent = (allContent, dataParsing)
parseEachString str (c:cs) dataParsing hasCheckfirstStrIsAnElem allContent
    | not hasCheckfirstStrIsAnElem =
        checkFrstStr str (skipSpaces 3 str) dataParsing allContent
    | otherwise = analyseBasicString c cs dataParsing allContent

analyseBasicString :: Char -> String -> DataParsing ->
    [PContent] -> ([PContent], DataParsing)
analyseBasicString c cs dataParsing allContent
    | strcmp newStr cs = parseEachString cs cs newDataParsed' True allContent
    | otherwise =
        parseEachString newStr newStr dataAfterLinkImg True allContent
    where
        (dataAfterLinkImg, newStr) = checkImgAndLink c cs dataParsing
        newDataParsed' = parseOneChar c dataParsing

initNewSection :: DataParsing -> [PContent] -> Int ->
    String -> (DataParsing, String, [PContent])
initNewSection dataParsing allContent levelSect rightPart =
    (finalData { levelSection = levelSect, typeToAdd = Section },
    rightPart, finalContent)
    where
        (newContent, newData) = tryAddParagraph dataParsing allContent
        (finalContent, finalData) =
            createSection levelSect rightPart newData newContent

initNewCodeblock :: DataParsing -> [PContent] ->
    String -> (DataParsing, String, [PContent])
initNewCodeblock dataParsing allContent rightPart =
    (finalDataParsed, rightPart, newContent)
    where
        (newfrstContent, newData) = tryAddParagraph dataParsing allContent
        newDataParsed = newData { isInCodeblock = not
            (isInCodeblock dataParsing), typeToAdd = CodeBlock }
        (finalDataParsed, newContent) =
            tryAddCodeBlock newDataParsed newfrstContent

initNewItem :: DataParsing -> [PContent] -> String ->
    String -> (DataParsing, String, [PContent])
initNewItem dataParsing allContent rightPart str =
    (finalData { levelItem = (levelItem dataParsing) + 1,
        typeToAdd = Item }, rightPart, finalContent)
    where
        (newContent, newData) = tryAddItem dataParsing allContent
        (finalContent, finalData) = createItem str rightPart newData newContent

checkFrstStr :: String -> String -> DataParsing ->
    [PContent] -> ([PContent], DataParsing)
checkFrstStr str st2 dataP content
    | not (strcmp oneStr str) = parseEachString "" "" d1 True oneContent
    | not (strcmp twoStr str) = parseEachString "" "" d2 True twoContent
    | not (strcmp threeStr str) = parseEachString "" "" d3 True threeContent
    | not (strcmp newStr str) = parseEachString "" "" d4 True newContent
    | otherwise = parseEachString newStr newStr d4 True newContent
    where
        (d1, oneStr, oneContent) = checkSectionOne str st2 dataP content
        (d2, twoStr, twoContent) = checkSectionTwo str st2 dataP content
        (d3, threeStr, threeContent) = checkSectionThree str st2 dataP content
        (d4, newStr, newContent) = checkfirstStrIsAnElem str st2 dataP content


checkfirstStrIsAnElem :: String -> String -> DataParsing ->
    [PContent] -> (DataParsing, String, [PContent])
checkfirstStrIsAnElem str stringSkipSpaces dataParsing allContent
    | Just (_, rightPart) <- isCodeBlock =
        initNewCodeblock dataParsing allContent rightPart
    | Just (_, rightPart) <- isItem =
        initNewItem dataParsing allContent rightPart str
    | otherwise = defineParagraphType dataParsing str allContent
    where
        isCodeBlock = parseStartCodeBlock str
        isItem = parseString "-" stringSkipSpaces

checkSectionOne :: String -> String -> DataParsing ->
    [PContent] -> (DataParsing, String, [PContent])
checkSectionOne str stringSkipSpaces dataParsing allContent
    | Just (_, rightPart) <- isSectionSix =
        initNewSection dataParsing allContent 6 rightPart
    | Just (_, rightPart) <- isSectionFive =
        initNewSection dataParsing allContent 5 rightPart
    | otherwise = (dataParsing, str, allContent)
    where
        isSectionSix = parseString "###### " stringSkipSpaces
        isSectionFive = parseString "##### " stringSkipSpaces

checkSectionTwo :: String -> String -> DataParsing ->
    [PContent] -> (DataParsing, String, [PContent])
checkSectionTwo str stringSkipSpaces dataParsing allContent
    | Just (_, rightPart) <- isSectionFour =
        initNewSection dataParsing allContent 4 rightPart
    | Just (_, rightPart) <- isSectionThree =
        initNewSection dataParsing allContent 3 rightPart
    | otherwise = (dataParsing, str, allContent)
    where
        isSectionFour = parseString "#### " stringSkipSpaces
        isSectionThree = parseString "### " stringSkipSpaces

checkSectionThree :: String -> String -> DataParsing ->
    [PContent] -> (DataParsing, String, [PContent])
checkSectionThree str stringSkipSpaces dataParsing allContent
    | Just (_, rightPart) <- isSectionTwo =
        initNewSection dataParsing allContent 2 rightPart
    | Just (_, rightPart) <- isSectionOne =
        initNewSection dataParsing allContent 1 rightPart
    | otherwise = (dataParsing, str, allContent)
    where
        isSectionTwo = parseString "## " stringSkipSpaces
        isSectionOne = parseString "# " stringSkipSpaces
