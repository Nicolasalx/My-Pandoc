--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Item
--

module ParseMarkdown.ParseElem.Item (createItem, tryAddItem) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..), PParagraph(..), PItemType(..), PItem(..))
import ParseMarkdown.ParseElem.SkipSpaces (skipSpaces)
import ParsingLib.Lib (parseString)
import ParseMarkdown.FormatText.CreateText (formattingElemParagraph)
import ParseMarkdown.ParseElem.Paragraph (createParagraph)

import Debug.Trace (trace)

tryAddItem :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddItem dataParsing allContent
    | typeToAdd dataParsing == Paragraph = createParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

frstElemNotSpaceOrHyphen :: String -> Int
frstElemNotSpaceOrHyphen [] = 0
frstElemNotSpaceOrHyphen (x:xs)
    | x == '-' = 1
    | x /= ' ' && x /= '-' = 2
    | otherwise = frstElemNotSpaceOrHyphen xs

determineDepthItem :: String -> Int -> (Int, String)
determineDepthItem str actualLevel = do
    let (level, newStr) = chooseIndexItem (frstElemNotSpaceOrHyphen str) str actualLevel
    (level, newStr)

chooseIndexItem :: Int -> String -> Int -> (Int, String)
chooseIndexItem 0 _ _ = (0, "")
chooseIndexItem 1 str actualLevel = do
    let stringSkipSpaces = skipSpaces 100 str
    case parseString "-" stringSkipSpaces of
        Just (_, rightPart) -> (determineDepthItem rightPart (actualLevel + 1))
        Nothing -> (0, "")
chooseIndexItem 2 str actualLevel = (actualLevel + 1, (skipSpaces 100 str))
chooseIndexItem _ _ _ = (0, "")

createItem :: String -> String -> DataParsing -> [PContent] -> ([PContent], DataParsing)
createItem initialStr rightPart dataParsing allContent =
    let (levItem, restStr) = determineDepthItem rightPart 0
    in
    if levItem > 0 || length(restStr) == 0
        then
            let newDataParsed = dataParsing { actualList = restStr, levelItem = levItem, preElemIsItem = True, insertItem = True }
                newItem = makeItem newDataParsed 0 levItem
            in
            (allContent, newItem { actualList = "", nbReturnLines = 0 })
        else
            (allContent, dataParsing { actualList = initialStr })

createTextItem :: DataParsing -> PParagraph
createTextItem dataParsing
    | length (actualList dataParsing) > 0 = PParagraph [formattingElemParagraph dataParsing]
    | otherwise = PParagraph []

createParagraphItem :: DataParsing -> PItemType
createParagraphItem dataParsing = do
    let newText = createTextItem dataParsing
    PParagraphItem newText

makeItem :: DataParsing -> Int -> Int -> DataParsing 
makeItem dataParsing actualIndex endIndex
    | actualIndex == endIndex = dataParsing { listItem = (listItem dataParsing) ++ [PItem [createParagraphItem dataParsing]] }
    | otherwise = makeItem dataParsing (actualIndex + 1) endIndex
