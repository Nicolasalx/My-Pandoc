--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- NoParentSection
--

module ParseMarkdown.ParseElem.NoParentSection (createNewSection) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..))
import ParseMarkdown.ParseElem.ParagraphType (initNewSection, tryAddFrstSection)
import ParseMarkdown.ParseElem.InsertInSection (insertInLastSection)

createNewSection :: Int -> String -> DataParsing -> [PContent] -> Bool -> ([PContent], DataParsing)
createNewSection levelSect titleSection dataParsing allContent isSectionOut
    | isSectionOut = do
        let newSect = initNewSection titleSection
            newContent = tryAddFrstSection levelSect newSect allContent
        (checkIndexAndInsert dataParsing levelSect newSect newContent)
    | otherwise = do
        let newSect = initNewSection titleSection
        let finalContent = loopInsertSection 1 1 levelSect newSect allContent
        (finalContent, dataParsing)

checkIndexAndInsert :: DataParsing -> Int -> PContent -> [PContent] -> ([PContent], DataParsing)
checkIndexAndInsert dataParsing levelSect newSect newContent
    | levelSect == 1 = (newContent, dataParsing)
    | otherwise = do
        let finalContent = loopInsertSection 1 1 (levelSect - 1) newSect newContent
        (finalContent, dataParsing)

loopInsertSection :: Int -> Int -> Int -> PContent -> [PContent] -> [PContent]
loopInsertSection limitIndex actualIndex maxIndex actualContent allContent
    | actualIndex == maxIndex = do
        (insertInLastSection actualContent allContent)
    | actualIndex == limitIndex = do
        let newContent = (insertInLastSection (initNewSection "") allContent)
        loopInsertSection limitIndex (actualIndex + 1) maxIndex actualContent newContent
    | otherwise = do
        let newContent =  (insertInLastSection (initNewSection "") allContent)
        loopInsertSection limitIndex (actualIndex + 1) maxIndex actualContent newContent
