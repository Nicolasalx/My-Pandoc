{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Bold
-}

module ParseMarkdown.FormatText.TextType.Bold (symbolBoldAlreadyOpen,
    symbolBoldNotOpen) where
import ParseMarkdown.DataStructMarkdown (TypeText(..),  DataText(..),
    ElemTextType(..))
import Content ()
import ParseMarkdown.FormatText.TryAddBasicList (tryAddBasicToList,
    fillInvalidPattern)

symbolBoldAlreadyOpen :: String -> DataText -> (String, DataText)
symbolBoldAlreadyOpen str dataText
    | null (basicStr dataText) || (precedentChar dataText /= ' ' &&
        last (basicStr dataText) /= ' ') =
        ([' '] ++ str, newData { isInBold = False, listText =
            listText newData ++ [TBold Bold] })
    | length str > 0 && head str /= ' ' =
        ([' '] ++ str, fillInvalidPattern 0 2 '*' dataText)
    | otherwise = ([' '] ++ str, fillInvalidPattern 0 2 '*' dataText)
    where
        newData = tryAddBasicToList dataText

symbolBoldNotOpen :: String -> DataText -> (String, DataText)
symbolBoldNotOpen str dataText
    | length str > 0 && head str /= ' ' =
        ([' '] ++ str, newData { isInBold = True,
            listText = listText newData ++ [TBold Bold] })
    | otherwise =
        (str, fillInvalidPattern 0 2 '*' dataText)
    where
        newData = tryAddBasicToList dataText

