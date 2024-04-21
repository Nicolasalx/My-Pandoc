--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- FormatList
--

module ParseMarkdown.FormatText.FormatList (formatLastList) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..), ElemTextType(..))
import Content ()

formatLastList :: DataText -> [ElemTextType] -> [ElemTextType] -> [ElemTextType]
formatLastList _ [] finalList = finalList
formatLastList dataText (x:xs) finalList
    | x == TBold Bold = do
        let (newData, newList) = setBoldList dataText x finalList
        formatLastList newData xs newList
    | x == TItalic Italic = do
        let (newData, newList) = setItalicList dataText x finalList
        formatLastList newData xs newList
    | x == TCode Code = do
        let (newData, newList) = setCodeList dataText x finalList
        formatLastList newData xs newList
    | otherwise = formatLastList dataText xs (finalList ++ [x])

removeElement :: ElemTextType -> [ElemTextType] -> [ElemTextType] -> [ElemTextType]
removeElement _ [] newList = newList
removeElement elemText (x:xs) newList
    | x == elemText && elemText == (TBold Bold) = removeElement elemText xs (newList ++ [(TString "**")])
    | x == elemText && elemText == (TItalic Italic) = removeElement elemText xs (newList ++ [(TString "*")])
    | otherwise = removeElement elemText xs (newList ++ [x])

setBoldList :: DataText -> ElemTextType -> [ElemTextType] -> (DataText, [ElemTextType])
setBoldList dataText actualElem actualListText
    | (isInCode dataText) && not (isInBold dataText) = 
        (dataText { isInBold = True }, removeElement actualElem actualListText [])
    | (isInCode dataText) && (isInBold dataText) = 
        (dataText { isInBold = False }, removeElement actualElem actualListText [])
    | not (isInBold dataText) = 
        (dataText { isInBold = True }, actualListText ++ [actualElem])
    | isInBold dataText = 
        (dataText { isInBold = False }, actualListText ++ [actualElem])
    | otherwise = 
        (dataText, actualListText)

setItalicList :: DataText -> ElemTextType -> [ElemTextType] -> (DataText, [ElemTextType])
setItalicList dataText actualElem actualListText
    | (isInCode dataText) && not (isInItalic dataText) = 
        (dataText { isInItalic = True }, removeElement actualElem actualListText [])
    | (isInCode dataText) && (isInItalic dataText) = 
        (dataText { isInItalic = False }, removeElement actualElem actualListText [])
    | not (isInItalic dataText) = 
        (dataText { isInItalic = True }, actualListText ++ [actualElem])
    | isInItalic dataText = 
        (dataText { isInItalic = False }, actualListText ++ [actualElem])
    | otherwise = 
        (dataText, actualListText)


setCodeList :: DataText -> ElemTextType -> [ElemTextType] -> (DataText, [ElemTextType])
setCodeList dataText actualElem actualListText
    | not (isInCode dataText) = (dataText { isInCode = True } , actualListText ++ [actualElem])
    | otherwise = (dataText { isInCode = False } , actualListText ++ [actualElem])
