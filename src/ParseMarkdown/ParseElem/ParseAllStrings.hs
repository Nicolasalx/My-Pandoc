--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseAllStrings
--

module ParseMarkdown.ParseElem.ParseAllStrings (parseAllString) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..))
import ParseMarkdown.ParseElem.ParseEachString (parseEachString)
import ParseMarkdown.ParseElem.TryAddElem (tryAddElemToContent, fillElemEmptyActualList)

parseAllString :: [String] -> DataParsing -> [PContent] -> IO (Either String [PContent], DataParsing)
parseAllString [] dataParsing allContent = return ((Right allContent), dataParsing)
parseAllString (x:xs) dataParsing allContent = do
    (content, stringParsed) <- parseEachString x x dataParsing False allContent
    (newContent, newData) <- tryAddElemToContent stringParsed content
    let newDataParsed = newData { nbReturnLines = nbReturnLines dataParsing + 1 }
    finalData <- fillElemEmptyActualList newDataParsed
    parseAllString xs finalData newContent
