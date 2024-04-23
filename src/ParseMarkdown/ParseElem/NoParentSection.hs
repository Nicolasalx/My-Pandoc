{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- NoParentSection
-}

module ParseMarkdown.ParseElem.NoParentSection (createNewSection) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..))
import ParseMarkdown.ParseElem.ParagraphType (
    initNewSection,
    tryAddFrstSection)
import ParseMarkdown.ParseElem.InsertInSection (insertInLastSection)

createNewSection :: Int -> String -> DataParsing ->
    [PContent] -> Bool -> ([PContent], DataParsing)
createNewSection levelSect titleSection dataParsing allContent isSectionOut
    | isSectionOut =
        (checkIndexAndInsert dataParsing levelSect (initNewSection
        titleSection) (tryAddFrstSection levelSect (initNewSection
        titleSection) allContent))
    | otherwise =
        (loopInsertSection 1 1 levelSect (initNewSection titleSection)
        allContent, dataParsing)

checkIndexAndInsert :: DataParsing -> Int -> PContent ->
    [PContent] -> ([PContent], DataParsing)
checkIndexAndInsert dataParsing 1 _ newContent = (newContent, dataParsing)
checkIndexAndInsert dataParsing levelSect newSect newContent =
    (loopInsertSection 1 1 (levelSect - 1) newSect newContent, dataParsing)

loopInsertSection :: Int -> Int -> Int -> PContent -> [PContent] -> [PContent]
loopInsertSection limitIndex actualIndex maxIndex actualContent allContent
    | actualIndex == maxIndex =
        insertInLastSection actualContent allContent
    | actualIndex == limitIndex =
        loopInsertSection limitIndex (actualIndex + 1) maxIndex
            actualContent (insertInLastSection (initNewSection "") allContent)
    | otherwise =
        loopInsertSection limitIndex (actualIndex + 1) maxIndex
            actualContent (insertInLastSection (initNewSection "") allContent)
