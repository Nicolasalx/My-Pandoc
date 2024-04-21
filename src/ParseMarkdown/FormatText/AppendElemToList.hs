--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AppendElemToList
--

module ParseMarkdown.FormatText.AppendElemToList (appendAllElem) where
import ParseMarkdown.DataStructMarkdown (TypeText(..), DataText(..), ElemTextType(..))
import Content (PText(..), PTextType(..), PBold(..), PItalic(..), PCode(..))

appendAllElem :: DataText -> [ElemTextType] -> IO DataText
appendAllElem dataText [] = return (dataText { contentText = (reversePText (contentText dataText)) })
appendAllElem dataText (x:xs)
    | x == (TBold Bold) && not (isInBold dataText) = do
        let newData = dataText { isInBold = True }
        newList <- findGoodPosition (indexListText newData) (PBoldText (PBold [])) (contentText newData)
        let finalData = newData { contentText = newList, indexListText = (indexListText newData) + 1 }
        appendAllElem finalData xs

    | x == (TBold Bold) && (isInBold dataText) = do
        let finalData = dataText { isInBold = False, indexListText = (indexListText dataText) - 1 }
        appendAllElem finalData xs
------------------------------------------------------------------------------------------
    | x == (TItalic Italic) && not (isInItalic dataText) = do
        let newData = dataText { isInItalic = True }
        newList <- findGoodPosition (indexListText newData) (PItalicText (PItalic [])) (contentText newData)
        let finalData = newData { contentText = newList, indexListText = (indexListText newData) + 1 }
        appendAllElem finalData xs

    | x == (TItalic Italic) && (isInItalic dataText) = do
        let finalData = dataText { isInItalic = False, indexListText = (indexListText dataText) - 1 }
        appendAllElem finalData xs
------------------------------------------------------------------------------------------
    | x == (TCode Code) && not (isInCode dataText) = do
        let newData = dataText { isInCode = True }
        newList <- findGoodPosition (indexListText newData) (PCodeText (PCode [])) (contentText newData)
        let finalData = newData { contentText = newList, indexListText = (indexListText newData) + 1 }
        appendAllElem finalData xs

    | x == (TCode Code) && (isInCode dataText) = do
        let finalData = dataText { isInCode = False, indexListText = (indexListText dataText) - 1 }
        appendAllElem finalData xs
------------------------------------------------------------------------------------------    
    | otherwise = case x of
        TString str -> do
            newList <- findGoodPosition (indexListText dataText) (PString str) (contentText dataText)
            appendAllElem (dataText { contentText = newList }) xs
        _ -> appendAllElem dataText xs

reversePText :: PText -> PText
reversePText (PText pTextTypes) = PText (reversePTextList pTextTypes)

reversePTextList :: [PTextType] -> [PTextType]
reversePTextList = reverse . map reversePTextType

reversePTextType :: PTextType -> PTextType
reversePTextType (PString s) = PString s
reversePTextType (PBoldText (PBold ts)) = PBoldText (PBold (reversePTextList ts))
reversePTextType (PItalicText (PItalic ts)) = PItalicText (PItalic (reversePTextList ts))
reversePTextType (PCodeText (PCode ts)) = PCodeText (PCode (reversePTextList ts))

findGoodPosition :: Int -> PTextType -> PText -> IO PText
findGoodPosition index actualElem (PText list) = do
    let newPText = (insertAtIndex index actualElem (list))
    return (PText newPText)

insertAtIndex :: Int -> PTextType -> [PTextType] -> [PTextType]
insertAtIndex index actualElem list
    | index == 0 = actualElem : list
    | otherwise = go index list
    where
        go 0 rest = actualElem : rest
        go n (x:xs) = case x of
            PBoldText (PBold y) -> (PBoldText (PBold (insertAtIndex (n - 1) actualElem y)) : xs)
            PItalicText (PItalic y) -> (PItalicText (PItalic (insertAtIndex (n - 1) actualElem y)) : xs)
            PCodeText (PCode y) -> (PCodeText (PCode (insertAtIndex (n - 1) actualElem y)) : xs)
            _ -> x : go n xs
        go _ [] = [actualElem]