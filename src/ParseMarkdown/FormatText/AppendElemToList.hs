{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AppendElemToList
-}

module ParseMarkdown.FormatText.AppendElemToList (appendAllElem) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..),
    ElemTextType(..))
import Content (PText(..), PTextType(..), PBold(..),
    PItalic(..), PCode(..))

boldInText :: DataText -> [ElemTextType] -> DataText
boldInText dataText xs = 
    appendAllElem finalData xs
    where
        newData = dataText { isInBold = True }
        newList = findGoodPosition (indexListText newData)
            (PBoldText (PBold [])) (contentText newData)
        finalData = newData { contentText = newList,
            indexListText = indexListText newData + 1 }

boldOutText :: DataText -> [ElemTextType] -> DataText
boldOutText dataText xs = 
    appendAllElem finalData xs
    where
        finalData = dataText { isInBold = False,
            indexListText = indexListText dataText - 1 }

italicInText :: DataText -> [ElemTextType] -> DataText
italicInText dataText xs = 
    appendAllElem finalData xs
    where
        newData = dataText { isInItalic = True }
        newList = findGoodPosition (indexListText newData)
            (PItalicText (PItalic [])) (contentText newData)
        finalData = newData { contentText = newList,
            indexListText = indexListText newData + 1 }
    
italicOutText :: DataText -> [ElemTextType] -> DataText
italicOutText dataText xs = 
    appendAllElem finalData xs
    where
        finalData = dataText { isInItalic = False,
            indexListText = indexListText dataText - 1 }

codeInText :: DataText -> [ElemTextType] -> DataText
codeInText dataText xs = 
    appendAllElem finalData xs
    where
        newData = dataText { isInCode = True }
        newList = findGoodPosition (indexListText newData)
            (PCodeText (PCode [])) (contentText newData)
        finalData = newData { contentText = newList,
            indexListText = indexListText newData + 1 }

codeOutText :: DataText -> [ElemTextType] -> DataText
codeOutText dataText xs = 
    appendAllElem finalData xs
    where
        finalData = dataText { isInCode = False,
            indexListText = indexListText dataText - 1 }

appendAllElem :: DataText -> [ElemTextType] -> DataText
appendAllElem dataText [] = (dataText { contentText =
    (reversePText (contentText dataText)) })
appendAllElem dataText (x:xs)
    | x == (TBold Bold) && not (isInBold dataText) = boldInText dataText xs
    | x == (TBold Bold) && (isInBold dataText) = boldOutText dataText xs
    | x == (TItalic Italic) && not (isInItalic dataText) =
        italicInText dataText xs
    | otherwise = appendElemOpt dataText x xs

appendElemOpt :: DataText -> ElemTextType -> [ElemTextType] -> DataText
appendElemOpt dataText element list
    | element == (TItalic Italic) && (isInItalic dataText) =
        italicOutText dataText list
    | element == (TCode Code) && not (isInCode dataText) =
        codeInText dataText list
    | element == (TCode Code) && (isInCode dataText) =
        codeOutText dataText list
    | otherwise = appendStr element dataText list

appendStr :: ElemTextType -> DataText -> [ElemTextType] -> DataText
appendStr (TString str) dataText xs =
    appendAllElem (dataText { contentText = newList }) xs
    where
        newList = findGoodPosition (indexListText dataText)
            (PString str) (contentText dataText)
appendStr _ dataText xs = appendAllElem dataText xs

reversePText :: PText -> PText
reversePText (PText pTextTypes) = PText (reversePTextList pTextTypes)

reversePTextList :: [PTextType] -> [PTextType]
reversePTextList = reverse . map reversePTextType

reversePTextType :: PTextType -> PTextType
reversePTextType (PString s) = PString s
reversePTextType (PBoldText (PBold ts)) =
    PBoldText (PBold (reversePTextList ts))
reversePTextType (PItalicText (PItalic ts)) =
    PItalicText (PItalic (reversePTextList ts))
reversePTextType (PCodeText (PCode ts)) =
    PCodeText (PCode (reversePTextList ts))

findGoodPosition :: Int -> PTextType -> PText -> PText
findGoodPosition index actualElem (PText list) =
    (PText (idx index actualElem list))

idx :: Int -> PTextType -> [PTextType] -> [PTextType]
idx 0 e list = e : list
idx index e list = go index list
    where
    go 0 rest = e : rest
    go n (PBoldText (PBold y) : xs) = PBoldText (PBold (idx (n - 1) e y)) : xs
    go n (PItalicText (PItalic y) : xs) =
        PItalicText (PItalic (idx (n - 1) e y)) : xs
    go n (PCodeText (PCode y) : xs) = PCodeText (PCode (idx (n - 1) e y)) : xs
    go n (x : xs) = x : go n xs
    go _ [] = [e]
