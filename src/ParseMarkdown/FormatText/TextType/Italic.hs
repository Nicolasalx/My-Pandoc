{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Italic
-}

module ParseMarkdown.FormatText.TextType.Italic (symbolItalicAlreadyOpen,
    symbolItalicNotOpen) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..),
    ElemTextType(..))
import Content ()
import ParseMarkdown.FormatText.TryAddBasicList (tryAddBasicToList,
    fillInvalidPattern)

symbolItalicAlreadyOpen :: String -> DataText -> (String, DataText)
symbolItalicAlreadyOpen str dataText
    | null (basicStr dataText) || (precedentChar dataText /= ' ' &&
        last (basicStr dataText) /= ' ') =
        ([' '] ++ str, newData { isInItalic = False, listText =
            listText newData ++ [TItalic Italic] })
    | length str > 0 && head str /= ' ' =
        ([' '] ++ str, fillInvalidPattern 0 1 '*' dataText)
    | otherwise = ([' '] ++ str, fillInvalidPattern 0 1 '*' dataText)
    where
        newData = tryAddBasicToList dataText

symbolItalicNotOpen :: String -> DataText -> (String, DataText)
symbolItalicNotOpen str dataText
    | length str > 0 && head str /= ' ' =
        ([' '] ++ str, newData { isInItalic = True,
            listText = listText newData ++ [TItalic Italic] })
    | otherwise =
        ([' '] ++ str, fillInvalidPattern 0 1 '*' dataText)
    where
        newData = tryAddBasicToList dataText
