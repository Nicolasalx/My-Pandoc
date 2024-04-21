--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Item
--

module ParseMarkdown.ParseElem.Item (createItem) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PContent(..))
import ParseMarkdown.ParseElem.SkipSpaces (skipSpaces)
import ParsingLib.Lib (parseString)
import ParseMarkdown.FormatText.CreateText (createText)

frstElemNotSpaceOrHyphen :: String -> Int
frstElemNotSpaceOrHyphen [] = 0
frstElemNotSpaceOrHyphen (x:xs)
    | x == '-' = 1
    | x /= ' ' && x /= '-' = 2
    | otherwise = frstElemNotSpaceOrHyphen xs

determineDepthItem :: String -> Int -> IO (Int, String)
determineDepthItem str actualLevel = do
    let index = frstElemNotSpaceOrHyphen str
    (newLevel, newStr) <- chooseIndexItem index str actualLevel
    return (newLevel, newStr)

chooseIndexItem :: Int -> String -> Int -> IO (Int, String)
chooseIndexItem 0 _ _ = return (0, "")
chooseIndexItem 1 str actualLevel = do
    let stringSkipSpaces = skipSpaces 100 str
    case parseString "-" stringSkipSpaces of
        Just (_, rightPart) -> (determineDepthItem rightPart (actualLevel + 1)) -- Nested Item
        Nothing -> return (0, "")

chooseIndexItem 2 str actualLevel = return (actualLevel + 1, (skipSpaces 100 str))
chooseIndexItem _ _ _ = return (0, "")

createItem :: String -> String -> DataParsing -> [PContent] -> IO ([PContent], DataParsing)
createItem initialStr rightPart dataParsing allContent = do
    (levItem, restStr) <- determineDepthItem rightPart 0
    if levItem > 0
        then do
            -- print ("LEVEL ITEM: " ++ show (levItem) ++ " / Str : " ++ restStr)
            let newDataParsed = dataParsing { actualList = restStr, levelItem = levItem, preElemIsItem = True, insertItem = True }
            endData <- createText newDataParsed
            -- print endData
            return (allContent, endData)    
    else do
        -- ! print ("Bad string after detection of item '-' STR: [" ++ initialStr ++ "]")
        return (allContent, dataParsing { actualList = initialStr })
