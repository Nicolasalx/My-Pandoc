--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- TryAddBasicList
--

module ParseMarkdown.FormatText.TryAddBasicList (tryAddBasicToList, fillInvalidPattern, parseBasicChar) where
import ParseMarkdown.DataStructMarkdown (DataText(..), ElemTextType(..))
import Content ()

parseBasicChar :: DataText -> Char -> DataText
parseBasicChar dataText c = dataText { basicStr = (basicStr dataText) ++ [c] }

fillInvalidPattern :: Int -> Int -> Char -> DataText -> DataText
fillInvalidPattern index limit c dataText
    | index == limit = dataText
    | otherwise = fillInvalidPattern (index + 1) limit c (endData { precedentChar = c })
    where
        newDataText = parseBasicChar dataText c
        endData = newDataText

tryAddBasicToList :: DataText -> DataText
tryAddBasicToList dataText
    | length (basicStr dataText) > 0 = dataText { listText = (listText dataText) ++ [(TString (basicStr dataText))], basicStr = "" }
    | otherwise = dataText
