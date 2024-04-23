{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- InsertInSection
-}

module ParseMarkdown.ParseElem.InsertInSection (
    checkInsertSection,
    insertInLastSection) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..), PSection(..))
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)

insertInLastSection :: PContent -> [PContent] -> [PContent]
insertInLastSection contentToAdd contents =
    reverse (insertInLastSection' contentToAdd (reverse contents))
    where
        insertInLastSection' :: PContent -> [PContent] -> [PContent]
        insertInLastSection' contentToAdd' [] = [contentToAdd']
        insertInLastSection' contentToAdd' (PSectionContent section : xs) =
            PSectionContent (section { section_content = insertInLastSection'
            contentToAdd' (section_content section) }) : xs
        insertInLastSection' contentToAdd' (x:xs) =
            x : insertInLastSection' contentToAdd' xs

checkInsertSection :: DataParsing -> PContent -> [PContent] -> [PContent]
checkInsertSection dataParsing actualContent allContent
    | levelSection dataParsing == 0 =
        addNewElemToContent actualContent allContent
    | otherwise = insertInLastSection actualContent allContent
