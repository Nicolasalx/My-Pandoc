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
