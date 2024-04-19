--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Code
--

module ParseMarkdown.FormatText.TextType.Code (symbolCodedAlreadyOpen, symbolCodeNotOpen) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..), ElemTextType(..))
import Content ()
import ParseMarkdown.FormatText.TryAddBasicList (tryAddBasicToList)

symbolCodedAlreadyOpen :: String -> DataText -> IO (String, DataText)
symbolCodedAlreadyOpen str dataText = do
    let tmpDataText = tryAddBasicToList dataText
        newData = tmpDataText { isInCode = False, listText = (listText tmpDataText) ++ [TCode Code] }
    return ([' '] ++ str, newData)

symbolCodeNotOpen :: String -> DataText -> IO (String, DataText)
symbolCodeNotOpen str dataText = do
    let endData = tryAddBasicToList dataText
        newData = endData { isInCode = True, listText = (listText endData) ++ [TCode Code] }
    return ([' '] ++ str, newData)
