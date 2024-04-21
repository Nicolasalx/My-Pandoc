--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- InsertInSection
--

module ParseMarkdown.ParseElem.InsertInSection (checkInsertSection, insertInLastSection) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..), PSection(..))
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)

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
