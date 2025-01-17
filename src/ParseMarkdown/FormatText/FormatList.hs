{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- FormatList
-}

module ParseMarkdown.FormatText.FormatList (formatLastList) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..), ElemTextType(..))
import Content ()

formatLastList :: DataText -> [ElemTextType] ->
    [ElemTextType] -> [ElemTextType]
formatLastList _ [] finalList = finalList
formatLastList dataText (x:xs) finalList
    | TBold Bold <- x = formatLastList newData xs newList
    | TItalic Italic <- x = formatLastList newData xs newList
    | TCode Code <- x = formatLastList newData xs newList
    | otherwise = formatLastList dataText xs (finalList ++ [x])
    where
        (newData, newList) = setTypeFormat dataText x finalList

setTypeFormat :: DataText -> ElemTextType ->
    [ElemTextType] -> (DataText, [ElemTextType])
setTypeFormat dataText (TBold boldText) finalList =
    setBoldList dataText (TBold boldText) finalList
setTypeFormat dataText (TItalic italicText) finalList =
    setItalicList dataText (TItalic italicText) finalList
setTypeFormat dataText (TCode codeText) finalList =
    setCodeList dataText (TCode codeText) finalList
setTypeFormat dataText _ finalList = (dataText, finalList)

removeElement :: ElemTextType -> [ElemTextType] ->
    [ElemTextType] -> [ElemTextType]
removeElement _ [] newList = newList
removeElement elemText (x:xs) newList
    | x == elemText && elemText == (TBold Bold) =
        removeElement elemText xs (newList ++ [(TString "**")])
    | x == elemText && elemText == (TItalic Italic) =
        removeElement elemText xs (newList ++ [(TString "*")])
    | otherwise = removeElement elemText xs (newList ++ [x])

setBoldList :: DataText -> ElemTextType ->
    [ElemTextType] -> (DataText, [ElemTextType])
setBoldList dataText actualElem actualListText
    | (isInCode dataText) && not (isInBold dataText) =
        (dataText { isInBold = True },
        removeElement actualElem actualListText [])
    | (isInCode dataText) && (isInBold dataText) = 
        (dataText { isInBold = False },
        removeElement actualElem actualListText [])
    | otherwise = setBoldListOpt dataText actualElem actualListText

setBoldListOpt :: DataText -> ElemTextType ->
    [ElemTextType] -> (DataText, [ElemTextType])
setBoldListOpt dataText actualElem actualListText
    | not (isInBold dataText) = 
        (dataText { isInBold = True }, actualListText ++ [actualElem])
    | isInBold dataText = 
        (dataText { isInBold = False }, actualListText ++ [actualElem])
    | otherwise = 
        (dataText, actualListText)

setItalicList :: DataText -> ElemTextType ->
    [ElemTextType] -> (DataText, [ElemTextType])
setItalicList dataText actualElem actualListText
    | (isInCode dataText) && not (isInItalic dataText) = 
        (dataText { isInItalic = True },
        removeElement actualElem actualListText [])
    | (isInCode dataText) && (isInItalic dataText) = 
        (dataText { isInItalic = False },
        removeElement actualElem actualListText [])
    | otherwise = setItalicListOpt dataText actualElem actualListText

setItalicListOpt :: DataText -> ElemTextType ->
    [ElemTextType] -> (DataText, [ElemTextType])
setItalicListOpt dataText actualElem actualListText
    | not (isInItalic dataText) =
        (dataText { isInItalic = True }, actualListText ++ [actualElem])
    | isInItalic dataText = 
        (dataText { isInItalic = False }, actualListText ++ [actualElem])
    | otherwise = 
        (dataText, actualListText)

setCodeList :: DataText -> ElemTextType ->
    [ElemTextType] -> (DataText, [ElemTextType])
setCodeList dataText actualElem actualListText
    | not (isInCode dataText) =
        (dataText { isInCode = True } , actualListText ++ [actualElem])
    | otherwise =
        (dataText { isInCode = False } , actualListText ++ [actualElem])
