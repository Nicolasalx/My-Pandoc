--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Section
--

module ParseMarkdown.ParseElem.Section (createSection) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..))
import ParseMarkdown.ParseElem.SkipSpaces (skipSpaces)
import ParseMarkdown.ParseElem.NoParentSection (createNewSection)

createSection :: Int -> String -> DataParsing -> [PContent] -> IO ([PContent], DataParsing)
createSection actualLevel titleSection dataParsing allContent
    | levelSection dataParsing == 0 = createNewSection actualLevel (skipSpaces 100 titleSection) dataParsing allContent True
    | (levelSection dataParsing) >= actualLevel = createNewSection actualLevel (skipSpaces 100 titleSection) dataParsing allContent True
    | otherwise = createNewSection (actualLevel - (levelSection dataParsing))  (skipSpaces 100 titleSection) dataParsing allContent False

