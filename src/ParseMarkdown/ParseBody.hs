--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseBody
--

module ParseMarkdown.ParseBody (parseBody) where
import Content (PContent(..), PText(..), PParagraph(..), PParagraphType(..), PSection(..), PCodeBlock(..), PTextType(..))
import ParsingLib.Lib (parseString, strcmp)
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import ParseMarkdown.ParseOneChar (parseOneChar)
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)

parseBody :: DataParsing -> IO (Either String [PContent])
parseBody dataParsing =
    parseAllString (remainingLines dataParsing) dataParsing [] >>= \(allContent, dataParsed) ->
    print dataParsed >>
    print "" >>
    print "" >>
    print allContent >>
    return (Left "ok")

parseAllString :: [String] -> DataParsing -> [PContent] -> IO (Either String [PContent], DataParsing)
parseAllString [] dataParsing allContent = return (Right allContent, dataParsing)
parseAllString (x:xs) dataParsing allContent = do
    (content, stringParsed) <- parseEachString x x dataParsing False allContent
    (newContent, newData) <- tryAddElemToContent stringParsed content
    let newDataParsed = newData { nbReturnLines = nbReturnLines dataParsing + 1 }
    finalData <- fillElemEmptyActualList newDataParsed
    parseAllString xs finalData newContent

tryAddElemToContent :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
tryAddElemToContent dataParsing allContent
    | typeToAdd dataParsing == Paragraph && (nbReturnLines dataParsing) > 0 && (length (actualList dataParsing)) > 0 = print ("Actual List: " ++ show (actualList dataParsing)) >> return (createParagraph dataParsing allContent)
    | typeToAdd dataParsing == Item = return (createItem dataParsing allContent)
    | otherwise = return (allContent, dataParsing)

fillElemEmptyActualList :: DataParsing -> IO DataParsing
fillElemEmptyActualList dataParsing
    | hasFillCodeBlock dataParsing == True = return dataParsing { actualCodeBlock = actualCodeBlock dataParsing ++ [(actualList dataParsing)], actualList = "" }
    | isInCodeblock dataParsing == True = return dataParsing { hasFillCodeBlock = True }
    -- Implement new line for item
    | otherwise = return dataParsing

------------------------------------------------------------------------------------------------------------
-----------------------------------       PARSE EACH STRING            -------------------------------------
------------------------------------------------------------------------------------------------------------

parseEachString :: String -> String -> DataParsing -> Bool -> [PContent] -> IO ([PContent], DataParsing)
parseEachString _ [] dataParsing _ allContent = return (allContent, dataParsing)
parseEachString str (c:cs) dataParsing hasCheckfirstStrIsAnElem allContent
    | not hasCheckfirstStrIsAnElem = checkFrstStr str dataParsing allContent
    | otherwise = analyseBasicString c cs dataParsing allContent

checkFrstStr :: String -> DataParsing -> [PContent] -> IO ([PContent] ,DataParsing)
checkFrstStr str dataParsing allContent = do
    (newDataParsing, newStr, newContent) <- checkfirstStrIsAnElem str dataParsing allContent
    if strcmp str newStr
        then
        parseEachString newStr newStr newDataParsing True newContent
    else
        parseEachString "" "" newDataParsing True newContent

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
        (finalContent, finalData) <- createSection 6 rightPart newData newContent
        return (finalData { levelSection = 6, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionFive = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 5 rightPart newData newContent
        return (finalData { levelSection = 5, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionFour = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 4 rightPart newData newContent
        return (finalData { levelSection = 4, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionThree = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 3 rightPart newData newContent
        return (finalData { levelSection = 3, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionTwo = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 2 rightPart newData newContent
        return (finalData { levelSection = 2, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionOne = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 1 rightPart newData newContent
        return (finalData { levelSection = 1, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isCodeBlock = do
        let (newfrstContent, newData) = tryAddParagraph dataParsing allContent
        let newDataParsed = newData { isInCodeblock = not (isInCodeblock dataParsing), typeToAdd = CodeBlock }
            (finalDataParsed, newContent) = tryAddCodeBlock newDataParsed newfrstContent
        return (finalDataParsed, rightPart, newContent)
    | Just (_, rightPart) <- isItem = do
        let (newContent, newData) = tryAddParagraph dataParsing allContent
        return (newData { levelItem = (levelItem dataParsing) + 1, typeToAdd = Item }, rightPart, newContent)
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

defineParagraphType :: DataParsing -> String -> [PContent] -> IO (DataParsing, String, [PContent])
defineParagraphType dataParsing str allContent
    | not (isInCodeblock dataParsing) && not (isInParagraph dataParsing) = return (dataParsing { typeToAdd = Paragraph }, str, allContent)
    | otherwise = return (dataParsing, str, allContent)

------------------------------------------------------------------------------------------------------------
-----------------------------------            PARAGRAPH               -------------------------------------
------------------------------------------------------------------------------------------------------------

{-
    The moment where i need to push to (paragraph dataParsing)

    When a new link
        - create PText with content of actualList => Check (length actualList) > 0
        - formatting PText => Check (length actualList) > 0
        - insert PText => Check (length actualList) > 0
        - create Link
        - insert Link
        ! Don't push now just insert in (paragraph dataParsing)

    When a new image
        - create PText with content of actualList => Check (length actualList) > 0 
        - formatting PText => Check (length actualList) > 0
        - insert PText => Check (length actualList) > 0
        - create Image
        - insert Image
        ! Don't push now just insert in (paragraph dataParsing)

    When i will "createParagraph"
        - create PText with content of actualList => Check (length actualList) > 0 
        - formatting PText => Check (length actualList) > 0
        - insert PText => Check (length actualList) > 0
        - Insert a new Paragraph in [PContent] with function "checkInsertSection" with data "paragraph dataParsing" 
-}

createParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
createParagraph dataParsing allContent = do
    let newContent = convertParagraphToContent (paragraph dataParsing)
        finalContent = checkInsertSection dataParsing newContent allContent
    (finalContent, dataParsing { paragraph = [], actualList = "" })

tryAddParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddParagraph dataParsing allContent
    | typeToAdd dataParsing == Paragraph = createParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

convertParagraphToContent :: [PParagraphType] -> PContent
convertParagraphToContent paragraphTypes = 
    PParagraphContent (PParagraph paragraphTypes)

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
parseStartCodeBlock str
    | startsWithThreeBackticksAndSpaces str = parseString "```" str
    | otherwise = Nothing

tryAddCodeBlock :: DataParsing -> [PContent] -> (DataParsing, [PContent])
tryAddCodeBlock dataParsing allContent
    | not (isInCodeblock dataParsing) = addCodeBlockToContent dataParsing allContent
    | otherwise = (dataParsing, allContent)

initializeCodeBlock :: DataParsing -> PContent
initializeCodeBlock dataParsing = PCodeBlockContent $ PCodeBlock ( actualCodeBlock dataParsing )

addCodeBlockToContent :: DataParsing -> [PContent] -> (DataParsing, [PContent])
addCodeBlockToContent dataParsing allContent = do
    let codeBlock = initializeCodeBlock dataParsing
        finalContent = checkInsertSection dataParsing codeBlock allContent
        newDataParsed = dataParsing { actualCodeBlock = [], hasFillCodeBlock = False }
    (newDataParsed, finalContent)

------------------------------------------------------------------------------------------------------------
-----------------------------------          SECTION                   -------------------------------------
------------------------------------------------------------------------------------------------------------

createSection :: Int -> String -> DataParsing -> [PContent] -> IO ([PContent], DataParsing)
createSection actualLevel titleSection dataParsing allContent
    | levelSection dataParsing == 0 = createNewSection actualLevel titleSection dataParsing allContent False
    | (levelSection dataParsing) >= actualLevel = createNewSection actualLevel titleSection dataParsing allContent True
    | otherwise = createNewSection (actualLevel - (levelSection dataParsing))  titleSection dataParsing allContent False

initNewSection :: String -> PContent
initNewSection titleSection = PSectionContent $ PSection { title = titleSection, section_content = [] }

-------------------------------- WHEN NO PARENT SECTION EXIST ------------------------------

createNewSection :: Int -> String -> DataParsing -> [PContent] -> Bool -> IO ([PContent], DataParsing)
createNewSection levelSection titleSection dataParsing allContent isSectionOut
    | isSectionOut = do 
        let newSect = initNewSection titleSection
            newContent = tryAddFrstSection levelSection newSect allContent
        (checkIndexAndInsert dataParsing levelSection newSect newContent)
    | otherwise = do
        let newSect = initNewSection titleSection
        finalContent <- loopInsertSection 1 1 levelSection newSect allContent
        return (finalContent, dataParsing)

checkIndexAndInsert :: DataParsing -> Int -> PContent -> [PContent] -> IO ([PContent], DataParsing)
checkIndexAndInsert dataParsing levelSection newSect newContent
    | levelSection == 1 = return (newContent, dataParsing)
    | otherwise = do
        finalContent <- loopInsertSection 1 1 (levelSection - 1) newSect newContent
        return (finalContent, dataParsing)

loopInsertSection :: Int -> Int -> Int -> PContent -> [PContent] -> IO [PContent]
loopInsertSection limitIndex actualIndex maxIndex actualContent allContent
    | actualIndex == maxIndex = do
        return (insertInLastSection actualContent allContent)
    | actualIndex == limitIndex = do
        let newContent = (insertInLastSection (initNewSection "") allContent)
        loopInsertSection limitIndex (actualIndex + 1) maxIndex actualContent newContent
    | otherwise = do
        let newContent =  (insertInLastSection (initNewSection "") allContent)
        loopInsertSection limitIndex (actualIndex + 1) maxIndex actualContent newContent

insertInLastSection :: PContent -> [PContent] -> [PContent]
insertInLastSection contentToAdd contents = reverse (insertInLastSection' contentToAdd (reverse contents))
    where
        insertInLastSection' :: PContent -> [PContent] -> [PContent]
        insertInLastSection' contentToAdd' [] = [contentToAdd']
        insertInLastSection' contentToAdd' (x:xs) =
            case x of
                PSectionContent section ->
                    let updatedSection = section { section_content = insertInLastSection' contentToAdd' (section_content section) }
                    in (PSectionContent updatedSection) : xs
                _ -> x : insertInLastSection' contentToAdd' xs

-- ! This function will carry the placement of each element
checkInsertSection :: DataParsing -> PContent -> [PContent] -> [PContent]
checkInsertSection dataParsing actualContent allContent
    | levelSection dataParsing == 0 = addNewElemToContent actualContent allContent
    | otherwise = insertInLastSection actualContent allContent

--------------------------- WHEN A PARENT SECTION ALREADY EXIST ---------------------------

tryAddFrstSection :: Int -> PContent -> [PContent] -> [PContent]
tryAddFrstSection levelSection content allContent
    | levelSection == 1 = addNewElemToContent content allContent
    | otherwise = addNewElemToContent (initNewSection "") allContent

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

-- Use checkInsertSection like in paragraph or codeblock

-- TODO LIST

-- ! When a codeBlock is Open but don't close -> The program wait the codeBlock close => Find a solution to this

-- ! If a Section is in codeBlock the result is this:
-- ```
-- # Section A
-- 
-- abc
-- 
-- ```

-- ! Problem with insertion in paragraph

-- => [PSectionContent (PSection {title = "Section A", section_content = [PCodeBlockContent (PCodeBlock ["","","abc",""])]})]

-- Begin the formatting text of a paragraph
-- Stock the actual paragraph in the dataParsing
    -- Insert link
    -- Insert image
    -- Insert text formatted

-- Begin Item
