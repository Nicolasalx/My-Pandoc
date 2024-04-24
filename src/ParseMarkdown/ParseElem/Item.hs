{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Item
-}

module ParseMarkdown.ParseElem.Item (createItem, tryAddItem) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..), PParagraph(..), PItemType(..), PItem(..))
import ParseMarkdown.ParseElem.SkipSpaces (skipSpaces)
import ParsingLib.Lib (parseString)
import ParseMarkdown.FormatText.CreateText (formattingElemParagraph)
import ParseMarkdown.ParseElem.Paragraph (createParagraph)

tryAddItem :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddItem dataParsing allContent
    | typeToAdd dataParsing == Paragraph =
        createParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

frstElemNotSpaceOrHyphen :: String -> Int
frstElemNotSpaceOrHyphen [] = 0
frstElemNotSpaceOrHyphen ('-':_) = 1
frstElemNotSpaceOrHyphen (x:xs)
    | x /= ' ' = 2
    | otherwise = frstElemNotSpaceOrHyphen xs

determineDepthItem :: String -> Int -> (Int, String)
determineDepthItem str actualLevel =
    chooseIndexItem (frstElemNotSpaceOrHyphen str) str actualLevel

chooseIndexItem :: Int -> String -> Int -> (Int, String)
chooseIndexItem 0 _ _ = (0, "")
chooseIndexItem 1 str actualLevel
    | Just (_, rp) <- parseString "-" stringSkipSpaces =
        determineDepthItem rp (actualLevel + 1)
    | otherwise = (0, "")
    where
        stringSkipSpaces = skipSpaces 100 str
chooseIndexItem 2 str actualLevel = (actualLevel + 1, skipSpaces 100 str)
chooseIndexItem _ _ _ = (0, "")

createItem :: String -> String -> DataParsing ->
    [PContent] -> ([PContent], DataParsing)
createItem initialStr rightPart dataParsing allContent
    | levItem > 0 || null restStr =
        (allContent, newItem { actualList = "", nbReturnLines = 0 })
    | otherwise =
        (allContent, dataParsing { actualList = initialStr })
    where
        (levItem, restStr) = determineDepthItem rightPart 0
        newDataParsed = dataParsing { actualList = restStr,
            levelItem = levItem,
            preElemIsItem = True, insertItem = True }
        newItem = makeItem newDataParsed 0 levItem

createTextItem :: DataParsing -> PParagraph
createTextItem dataParsing
    | length (actualList dataParsing) > 0 =
        PParagraph [formattingElemParagraph dataParsing]
    | otherwise = PParagraph []

createParagraphItem :: DataParsing -> PItemType
createParagraphItem dataParsing =
    PParagraphItem (createTextItem dataParsing)

makeItem :: DataParsing -> Int -> Int -> DataParsing 
makeItem dataParsing actualIndex endIndex
    | actualIndex == endIndex = dataParsing { listItem =
        (listItem dataParsing) ++ [PItem [createParagraphItem dataParsing]] }
    | otherwise = makeItem dataParsing (actualIndex + 1) endIndex
