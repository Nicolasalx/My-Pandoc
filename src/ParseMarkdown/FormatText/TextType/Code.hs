{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Code
-}

module ParseMarkdown.FormatText.TextType.Code (symbolCodedAlreadyOpen,
    symbolCodeNotOpen) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..),
    ElemTextType(..))
import Content ()
import ParseMarkdown.FormatText.TryAddBasicList (tryAddBasicToList)

symbolCodedAlreadyOpen :: String -> DataText -> (String, DataText)
symbolCodedAlreadyOpen str dataText =
    ([' '] ++ str, newData { isInCode = False,
        listText = listText newData ++ [TCode Code] })
    where
        newData = tryAddBasicToList dataText

symbolCodeNotOpen :: String -> DataText -> (String, DataText)
symbolCodeNotOpen str dataText =
    ([' '] ++ str, newData { isInCode = True,
        listText = listText newData ++ [TCode Code] })
    where
        newData = tryAddBasicToList dataText

