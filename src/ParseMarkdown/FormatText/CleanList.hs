{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- CleanList
-}

module ParseMarkdown.FormatText.CleanList (closeAllDelim) where
import ParseMarkdown.DataStructMarkdown (
    TypeText(..),
    DataText(..),
    ElemTextType(..))
import Content ()

closeAllDelim :: DataText -> DataText
closeAllDelim dataText =
    checkRemoveCode $ checkRemoveItalic $ checkRemoveBold dataText

checkRemoveBold :: DataText -> DataText
checkRemoveBold dataText
    | isInBold dataText = 
        dataText { listText =
            removeLastDelim (TBold Bold) (listText dataText) [] "**" }
    | otherwise = dataText

checkRemoveItalic :: DataText -> DataText
checkRemoveItalic dataText
    | isInItalic dataText = 
        dataText { listText =
            removeLastDelim (TItalic Italic) (listText dataText) [] "*" }
    | otherwise = dataText

checkRemoveCode :: DataText -> DataText
checkRemoveCode dataText
    | isInCode dataText = 
        dataText { listText =
            removeLastDelim (TCode Code) (listText dataText) [] "`" }
    | otherwise = dataText

removeLastDelim :: ElemTextType -> [ElemTextType] ->
    [ElemTextType] -> String -> [ElemTextType]
removeLastDelim delimToDelete basicList finalList elemToReplace =
    replaceLastDelim delimToDelete basicList finalList elemToReplace

replaceLastDelim :: ElemTextType -> [ElemTextType] ->
    [ElemTextType] -> String -> [ElemTextType]
replaceLastDelim _ [] finalList _ = finalList
replaceLastDelim delimToDelete (x:xs) finalList elemToReplace
    | x == delimToDelete =
        replaceLastDelim delimToDelete xs
        (finalList ++ [TString elemToReplace]) elemToReplace 
    | otherwise =
        replaceLastDelim delimToDelete xs (finalList ++ [x]) elemToReplace
