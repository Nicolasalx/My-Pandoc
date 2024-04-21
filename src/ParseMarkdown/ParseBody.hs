--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseBody
--

module ParseMarkdown.ParseBody (parseBody, skipSpaces) where
import Content (PContent(..), PParagraph(..), PParagraphType(..), PSection(..), PCodeBlock(..), PBody(..))
import ParsingLib.Lib (parseString, strcmp)
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import ParseMarkdown.ParseOneChar (parseOneChar)
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)
import ParseMarkdown.FormatText.CreateText (createText)

parseBody :: DataParsing -> IO (Either String PBody)
parseBody dataParsing =
    parseAllString (remainingLines dataParsing) dataParsing [] >>= \(allContent, newDataParsed) ->
    case allContent of
        Left err -> return (Left err)
        Right contents -> do
            dataPars <- createText newDataParsed
            (newContent, _) <- tryAddParagraph dataPars contents
            return (Right (PBody newContent))

parseAllString :: [String] -> DataParsing -> [PContent] -> IO (Either String [PContent], DataParsing)
parseAllString [] dataParsing allContent = return ((Right allContent), dataParsing)
parseAllString (x:xs) dataParsing allContent = do
    (content, stringParsed) <- parseEachString x x dataParsing False allContent
    (newContent, newData) <- tryAddElemToContent stringParsed content
    let newDataParsed = newData { nbReturnLines = nbReturnLines dataParsing + 1 }
    finalData <- fillElemEmptyActualList newDataParsed
    parseAllString xs finalData newContent

tryAddElemToContent :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
tryAddElemToContent dataParsing allContent
    | typeToAdd dataParsing == Paragraph && insertLinkOrImage dataParsing && (length (actualList dataParsing)) > 0 = do
        newDataParsed <- createText (dataParsing { insertLinkOrImage = False })
        (createParagraph newDataParsed allContent)
    | typeToAdd dataParsing == Paragraph && insertLinkOrImage dataParsing = do
        (createParagraph (dataParsing { insertLinkOrImage = False }) allContent)
    | typeToAdd dataParsing == Paragraph && (nbReturnLines dataParsing) > 0 && (length (actualList dataParsing)) > 0 = do
        newDataParsed <- createText dataParsing
        (createParagraph newDataParsed allContent)
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
    | Just (_, rightPart) <- parseString "![" str = do
        newData <- parseOneChar c dataParsing
        return (newData { isInAltImage = True }, rightPart)
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
        (newContent, newData) <- tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 6 rightPart newData newContent
        return (finalData { levelSection = 6, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionFive = do
        (newContent, newData) <- tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 5 rightPart newData newContent
        return (finalData { levelSection = 5, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionFour = do
        (newContent, newData) <- tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 4 rightPart newData newContent
        return (finalData { levelSection = 4, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionThree = do
        (newContent, newData) <- tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 3 rightPart newData newContent
        return (finalData { levelSection = 3, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionTwo = do
        (newContent, newData) <- tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 2 rightPart newData newContent
        return (finalData { levelSection = 2, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isSectionOne = do
        (newContent, newData) <- tryAddParagraph dataParsing allContent
        (finalContent, finalData) <- createSection 1 rightPart newData newContent
        return (finalData { levelSection = 1, typeToAdd = Section }, rightPart, finalContent)
    | Just (_, rightPart) <- isCodeBlock = do
        (newfrstContent, newData) <- tryAddParagraph dataParsing allContent
        let newDataParsed = newData { isInCodeblock = not (isInCodeblock dataParsing), typeToAdd = CodeBlock }
            (finalDataParsed, newContent) = tryAddCodeBlock newDataParsed newfrstContent
        return (finalDataParsed, rightPart, newContent)
    | Just (_, rightPart) <- isItem = do
        -- ! Item
        (newContent, newData) <- tryAddParagraph dataParsing allContent
        createItem str rightPart newData newContent
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
-----------------------------------            ITEM                    -------------------------------------
------------------------------------------------------------------------------------------------------------

frstElemNotSpaceOrHyphen :: String -> Int
frstElemNotSpaceOrHyphen [] = 0
frstElemNotSpaceOrHyphen (x:xs)
    | x == '-' = 1
    | x /= ' ' && x /= '-' = 2
    | otherwise = frstElemNotSpaceOrHyphen xs

determineDepthItem :: String -> Int -> IO (Int, String)
determineDepthItem str actualLevel = do
    let index = frstElemNotSpaceOrHyphen str
    (newLevel, newStr) <- chooseIndexItem index str actualLevel
    return (newLevel, newStr)

chooseIndexItem :: Int -> String -> Int -> IO (Int, String)
chooseIndexItem 0 _ _ = return (0, "")
chooseIndexItem 1 str actualLevel = do
    let stringSkipSpaces = skipSpaces 100 str
    case parseString "-" stringSkipSpaces of
        Just (_, rightPart) -> (determineDepthItem rightPart (actualLevel + 1)) -- Nested Item

        Nothing -> return (0, "")
chooseIndexItem 2 str actualLevel = return (actualLevel + 1, (skipSpaces 100 str))
chooseIndexItem _ _ _ = return (0, "")

createItem :: String -> String -> DataParsing -> [PContent] -> IO ([PContent], DataParsing)
createItem initialStr rightPart dataParsing allContent = do
    (levelItem, restStr) <- determineDepthItem rightPart 0
    if levelItem > 0
        then do
            -- ! print ("LEVEL ITEM: " ++ show (levelItem) ++ " / Str : " ++ restStr)
            let newDataParsed = dataParsing { actualList = restStr, levelItem = levelItem, preElemIsItem = True }

            -- Transformer restStr en 1 paragraphe (Assez complexe)
            -- Il juste prendre en compte le level de l'item dans cette fonction
            -- Après on laisse remplir (paragraph dataParsing)

            -- ! En dehors de cette fonction
            -- A la fin de cette string (il faut trouver un moyen de détecter quand la string est fini)
            -- Ensuite récupère (paragraph dataParsing)
            -- Insérer le paragraphe dans le dernier (Item listItem) avec le (levelItem dataParsing)

            return (allContent, dataParsing { actualList = "" })
  
    else do
        -- ! print ("Bad string after detection of item '-' STR: [" ++ initialStr ++ "]")
        return (allContent, dataParsing { actualList = initialStr })

------------------------------------------------------------------------------------------------------------
-----------------------------------            PARAGRAPH               -------------------------------------
------------------------------------------------------------------------------------------------------------

createParagraph :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
createParagraph dataParsing allContent
    | length (paragraph dataParsing) > 0 = do
        let newContent = convertParagraphToContent (paragraph dataParsing)
            finalContent = checkInsertSection dataParsing newContent allContent
        return (finalContent, dataParsing { paragraph = [], actualList = "" })
    | otherwise = return (allContent, dataParsing)

tryAddParagraph :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
tryAddParagraph dataParsing allContent
    | typeToAdd dataParsing == Paragraph = createParagraph dataParsing allContent
    | otherwise = return (allContent, dataParsing)

convertParagraphToContent :: [PParagraphType] -> PContent
convertParagraphToContent paragraphTypes = 
    PParagraphContent (PParagraph paragraphTypes)

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
    | levelSection dataParsing == 0 = createNewSection actualLevel (skipSpaces 100 titleSection) dataParsing allContent True
    | (levelSection dataParsing) >= actualLevel = createNewSection actualLevel (skipSpaces 100 titleSection) dataParsing allContent True
    | otherwise = createNewSection (actualLevel - (levelSection dataParsing))  (skipSpaces 100 titleSection) dataParsing allContent False

initNewSection :: String -> PContent
initNewSection titleSection = PSectionContent $ PSection { title = titleSection, section_content = [] }

-------------------------------- WHEN NO PARENT SECTION EXIST ------------------------------

createNewSection :: Int -> String -> DataParsing -> [PContent] -> Bool -> IO ([PContent], DataParsing)
createNewSection levelSect titleSection dataParsing allContent isSectionOut
    | isSectionOut = do
        let newSect = initNewSection titleSection
            newContent = tryAddFrstSection levelSect newSect allContent
        (checkIndexAndInsert dataParsing levelSect newSect newContent)
    | otherwise = do
        let newSect = initNewSection titleSection
        finalContent <- loopInsertSection 1 1 levelSect newSect allContent
        return (finalContent, dataParsing)

checkIndexAndInsert :: DataParsing -> Int -> PContent -> [PContent] -> IO ([PContent], DataParsing)
checkIndexAndInsert dataParsing levelSect newSect newContent
    | levelSect == 1 = return (newContent, dataParsing)
    | otherwise = do
        finalContent <- loopInsertSection 1 1 (levelSect - 1) newSect newContent
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
tryAddFrstSection levelSect content allContent
    | levelSect == 1 = addNewElemToContent content allContent
    | otherwise = addNewElemToContent (initNewSection "") allContent

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

-- TODO LIST

-- ! When a codeBlock is Open but don't close -> The program wait the codeBlock close => Find a solution to this
    -- ! Solution : Make a function to check if the codeblock is closed

-- ! If a Section is in codeBlock the result is this: [PSectionContent (PSection {title = "Section A", section_content = [PCodeBlockContent (PCodeBlock ["","","abc",""])]})]
-- ```
-- # Section A
-- 
-- abc
-- 
-- ```

-- ! If a [ is Open but not close (if it's not a link i will try to get all the line and not continue)
    -- ! Solution : Make a function to check if the link is closed 

-- ! Add a section with ====