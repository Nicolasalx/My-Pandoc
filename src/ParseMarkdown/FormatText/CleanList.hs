--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- CleanList
--

module ParseMarkdown.FormatText.CleanList (closeAllDelim) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..), ElemTextType(..))
import Content ()

closeAllDelim :: DataText -> IO DataText
closeAllDelim dataText = do
    dataBold <- checkRemoveBold dataText
    dataItalic <- checkRemoveItalic dataBold
    dataCode <- checkRemoveCode dataItalic
    return dataCode

checkRemoveBold :: DataText -> IO DataText
checkRemoveBold dataText
    | isInBold dataText = do
        updatedList <- (removeLastDelim (TBold Bold) (listText dataText) [] "**")
        return dataText { listText = updatedList }
    | otherwise = return dataText

checkRemoveItalic :: DataText -> IO DataText
checkRemoveItalic dataText
    | isInItalic dataText = do
        updatedList <- (removeLastDelim (TItalic Italic) (listText dataText) [] "*")
        return dataText { listText = updatedList }
    | otherwise = return dataText

checkRemoveCode :: DataText -> IO DataText
checkRemoveCode dataText
    | isInCode dataText = do
        updatedList <- (removeLastDelim (TCode Code) (listText dataText) [] "`")
        return dataText { listText = updatedList }
    | otherwise = return dataText

removeLastDelim :: ElemTextType -> [ElemTextType] -> [ElemTextType] -> String -> IO [ElemTextType]
removeLastDelim delimToDelete basicList finalList elemToReplace = replaceLastDelim delimToDelete basicList finalList elemToReplace

replaceLastDelim :: ElemTextType -> [ElemTextType] -> [ElemTextType] -> String -> IO [ElemTextType]
replaceLastDelim _ [] finalList _ = return finalList
replaceLastDelim delimToDelete (x:xs) finalList elemToReplace
    | x == delimToDelete = replaceLastDelim delimToDelete xs (finalList ++ [TString elemToReplace]) elemToReplace 
    | otherwise = replaceLastDelim delimToDelete xs (finalList ++ [x]) elemToReplace 
