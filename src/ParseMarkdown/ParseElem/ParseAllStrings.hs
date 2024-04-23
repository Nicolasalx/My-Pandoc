{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseAllStrings
-}

module ParseMarkdown.ParseElem.ParseAllStrings (parseAllString) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..))
import ParseMarkdown.ParseElem.ParseEachString (parseEachString)
import ParseMarkdown.ParseElem.TryAddElem (tryAddElemToContent,
    fillElemEmptyActualList)

parseAllString :: [String] -> DataParsing ->
    [PContent] -> (Either String [PContent], DataParsing)
parseAllString [] dataParsing allContent = (Right allContent, dataParsing)
parseAllString (x:xs) dataParsing allContent =
    parseAllString xs finalData newContent
    where
        (content, stringParsed) =
            parseEachString x x dataParsing False allContent
        (newContent, newData) = tryAddElemToContent stringParsed content
        newDataParsed =
            newData { nbReturnLines = nbReturnLines dataParsing + 1 }
        finalData = fillElemEmptyActualList newDataParsed
