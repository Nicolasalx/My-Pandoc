--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- CleanList
--

module ParseMarkdown.FormatText.CleanList (closeAllDelim) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..), ElemTextType(..))
import Content ()

closeAllDelim :: DataText -> DataText
closeAllDelim dataText = do
    let dataBold = checkRemoveBold dataText
        dataItalic = checkRemoveItalic dataBold
        dataCode = checkRemoveCode dataItalic
    dataCode

checkRemoveBold :: DataText -> DataText
checkRemoveBold dataText
    | isInBold dataText = do
        let updatedList = removeLastDelim (TBold Bold) (listText dataText) [] "**"
        dataText { listText = updatedList }
    | otherwise = dataText

checkRemoveItalic :: DataText -> DataText
checkRemoveItalic dataText
    | isInItalic dataText = do
        let updatedList = removeLastDelim (TItalic Italic) (listText dataText) [] "*"
        dataText { listText = updatedList }
    | otherwise = dataText

checkRemoveCode :: DataText -> DataText
checkRemoveCode dataText
    | isInCode dataText = do
        let updatedList = removeLastDelim (TCode Code) (listText dataText) [] "`"
        dataText { listText = updatedList }
    | otherwise = dataText

removeLastDelim :: ElemTextType -> [ElemTextType] -> [ElemTextType] -> String -> [ElemTextType]
removeLastDelim delimToDelete basicList finalList elemToReplace = replaceLastDelim delimToDelete basicList finalList elemToReplace

replaceLastDelim :: ElemTextType -> [ElemTextType] -> [ElemTextType] -> String -> [ElemTextType]
replaceLastDelim _ [] finalList _ = finalList
replaceLastDelim delimToDelete (x:xs) finalList elemToReplace
    | x == delimToDelete = replaceLastDelim delimToDelete xs (finalList ++ [TString elemToReplace]) elemToReplace 
    | otherwise = replaceLastDelim delimToDelete xs (finalList ++ [x]) elemToReplace 
