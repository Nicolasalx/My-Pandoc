--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Bold
--

module ParseMarkdown.FormatText.TextType.Bold (symbolBoldAlreadyOpen, symbolBoldNotOpen) where
import ParseMarkdown.DataStructMarkdown (TypeText(..),  DataText(..), ElemTextType(..))
import Content ()
import ParseMarkdown.FormatText.TryAddBasicList (tryAddBasicToList, fillInvalidPattern)

symbolBoldAlreadyOpen :: String -> DataText -> IO (String, DataText)
symbolBoldAlreadyOpen str dataText = do
    if ((length (basicStr dataText) == 0  || ((precedentChar dataText) /= ' ') && (last (basicStr dataText) /= ' ')))
        then do
            let tmpDataText = tryAddBasicToList dataText
                newData = tmpDataText { isInBold = False, listText = (listText tmpDataText) ++ [TBold Bold] }
            return ([' '] ++ str, newData)
    else do
        let newData = fillInvalidPattern 0 2 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return ([' '] ++ str, newData)
        else
            return ([' '] ++ str, newData)

symbolBoldNotOpen :: String -> DataText -> IO (String, DataText)
symbolBoldNotOpen str dataText = do
    if ((length (str) > 0 && head (str) /= ' '))
        then do
            let endData = tryAddBasicToList dataText
                newData = endData { isInBold = True, listText = (listText endData) ++ [TBold Bold] }
            return ([' '] ++str, newData)
    else do
        let newData = fillInvalidPattern 0 2 '*' dataText
        if ((length (str) > 0 && head (str) /= ' '))
            then return (str, newData)
        else
            return ([' '] ++str, newData)
