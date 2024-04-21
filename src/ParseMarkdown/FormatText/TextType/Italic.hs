--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Italic
--

module ParseMarkdown.FormatText.TextType.Italic (symbolItalicAlreadyOpen, symbolItalicNotOpen) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..), ElemTextType(..))
import Content ()
import ParseMarkdown.FormatText.TryAddBasicList (tryAddBasicToList, fillInvalidPattern)

symbolItalicAlreadyOpen :: String -> DataText -> (String, DataText)
symbolItalicAlreadyOpen str dataText = do
    if ((length (basicStr dataText) == 0  || (precedentChar dataText) /= ' ' && (last (basicStr dataText) /= ' ')))
        then do
            let tmpDataText = tryAddBasicToList dataText
                newData = tmpDataText { isInItalic = False, listText = (listText tmpDataText) ++ [TItalic Italic] }
            ([' '] ++str, newData)
    else do
        let newData = fillInvalidPattern 0 1 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then ([' '] ++ str, newData)
        else
            ([' '] ++ str, newData)

symbolItalicNotOpen :: String -> DataText -> (String, DataText)
symbolItalicNotOpen str dataText = do
    if ((length (str) > 0 && head (str) /= ' '))
        then do
            let endData = tryAddBasicToList dataText
                newData = endData { isInItalic = True, listText = (listText endData) ++ [TItalic Italic] }

            ([' '] ++ str, newData)
    else do
        let newData = fillInvalidPattern 0 1 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then ([' '] ++ str, newData)
        else
            ([' '] ++ str, newData)
