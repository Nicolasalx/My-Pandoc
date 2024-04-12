--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseBody
--

module ParseMarkdown.ParseBody (parseBody) where
import Content (PContent(..), PText(..), PParagraph(..), PParagraphType(..))
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import ParseMarkdown.ParseOneChar (parseOneChar)
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)

parseBody :: DataParsing -> IO (Either String [PContent])
parseBody dataParsing = do
    let allContent = []
    (allContent, dataParsed) <- parseAllString (remainingLines dataParsing) dataParsing allContent
    print allContent
    return (Left "ok")

parseAllString :: [String] -> DataParsing -> [PContent] -> IO (Either String [PContent], DataParsing)
parseAllString [] dataParsing allContent = return (Right allContent, dataParsing)
parseAllString (x:xs) dataParsing allContent =
    parseEachString x dataParsing >>= \stringParsed ->
    (\(newContent, newDataParsed) ->
        return (newDataParsed { nbReturnLines = nbReturnLines dataParsing + 1 }, newContent)
    ) (tryAddElemToContent stringParsed allContent) >>= \(newDataParsed, newContent) ->
    parseAllString xs newDataParsed newContent

parseEachString :: String -> DataParsing -> IO DataParsing
parseEachString [] dataParsing = return dataParsing
parseEachString (c:cs) dataParsing =
    parseOneChar c dataParsing >>= \dataParsed ->
    parseEachString cs dataParsed

tryAddElemToContent :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddElemToContent dataParsing allContent
    | not (isInLink dataParsing) && not (isInImage dataParsing) && not (isInCodeblock dataParsing) && (nbReturnLines dataParsing) > 0 = addNewParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

------------------------------------------------------------------------------------------------------------
-----------------------------------            PARAGRAPH               -------------------------------------
------------------------------------------------------------------------------------------------------------

addNewParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
addNewParagraph dataParsing allContent =
    (addNewElemToContent actualContent allContent, parsedData)
    where
        actualContent = initializePParagraphContent dataParsing
        parsedData = dataParsing {actualList = ""}

initializePText :: DataParsing -> PText
initializePText dataParsing = PText [Right (actualList dataParsing)]

initializePParagraphContent :: DataParsing -> PContent
initializePParagraphContent dataParsing =
    PParagraphContent $ PParagraph [PTextParagraph $ initializePText dataParsing]

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
