{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Codeblock
-}

module ParseMarkdown.ParseElem.Codeblock (
    tryAddCodeBlock,
    parseStartCodeBlock) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..), PCodeBlock(..))
import ParsingLib.Lib (parseString)
import ParseMarkdown.ParseElem.InsertInSection (checkInsertSection)

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
    | not (isInCodeblock dataParsing) =
        addCodeBlockToContent dataParsing allContent
    | otherwise = (dataParsing, allContent)

initializeCodeBlock :: DataParsing -> PContent
initializeCodeBlock dataParsing =
    PCodeBlockContent $ PCodeBlock ( actualCodeBlock dataParsing )

addCodeBlockToContent :: DataParsing -> [PContent] -> (DataParsing, [PContent])
addCodeBlockToContent dataParsing allContent =
    (dataParsing { actualCodeBlock = [], hasFillCodeBlock = False },
    checkInsertSection dataParsing
    (initializeCodeBlock dataParsing) allContent)
