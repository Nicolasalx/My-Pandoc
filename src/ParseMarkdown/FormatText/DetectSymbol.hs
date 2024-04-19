--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DetectSymbol
--

module ParseMarkdown.FormatText.DetectSymbol (detectSymbol) where
import ParseMarkdown.DataStructMarkdown (DataText(..))
import Content ()
import ParseMarkdown.FormatText.TextType.Bold (symbolBoldAlreadyOpen, symbolBoldNotOpen)
import ParseMarkdown.FormatText.TextType.Italic (symbolItalicAlreadyOpen, symbolItalicNotOpen)
import ParseMarkdown.FormatText.TextType.Code (symbolCodedAlreadyOpen, symbolCodeNotOpen)
import ParseMarkdown.FormatText.TryAddBasicList (parseBasicChar)
import ParsingLib.Lib (parseString)

detectSymbol :: String -> DataText -> IO (String, DataText)
detectSymbol str dataText
    | Just (_, rightPart) <- isBold, not (isInItalic dataText) = do
        if isInBold dataText
            then
                (symbolBoldAlreadyOpen rightPart dataText) 
        else
            (symbolBoldNotOpen rightPart dataText)

    | Just (_, rightPart) <- isItalic = do
        if isInItalic dataText
            then
                (symbolItalicAlreadyOpen rightPart dataText) 
        else
            (symbolItalicNotOpen rightPart dataText)
    | Just (_, rightPart) <- isBold = do
        if isInBold dataText
            then
                (symbolBoldAlreadyOpen rightPart dataText) 
        else
            (symbolBoldNotOpen rightPart dataText)

    | Just (_, rightPart) <- isCode = do
        if isInCode dataText
            then
                (symbolCodedAlreadyOpen rightPart dataText) 
        else
            (symbolCodeNotOpen rightPart dataText)

    | otherwise = do
        if (length str > 0)
            then do
                let newDataText = parseBasicChar dataText (head str)
                return (str, newDataText)
        else
            return (str, dataText)            
    where
        isBold = parseString "**" str
        isItalic = parseString "*" str
        isCode = parseString "`" str
