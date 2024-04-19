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

checkBold :: String -> DataText -> IO (String, DataText)
checkBold str dataText
    | isInBold dataText = symbolBoldAlreadyOpen str dataText
    | otherwise = symbolBoldNotOpen str dataText

checkItalic :: String -> DataText -> IO (String, DataText)
checkItalic str dataText
    | isInItalic dataText = symbolItalicAlreadyOpen str dataText
    | otherwise = symbolItalicNotOpen str dataText

checkCode :: String -> DataText -> IO (String, DataText)
checkCode str dataText
    | isInCode dataText = symbolCodedAlreadyOpen str dataText
    | otherwise = symbolCodeNotOpen str dataText

detectSymbol :: String -> DataText -> IO (String, DataText)
detectSymbol str dataText
    | Just (_, rightPart) <- isBold, not (isInItalic dataText) = checkBold rightPart dataText
    | Just (_, rightPart) <- isItalic = checkItalic rightPart dataText
    | Just (_, rightPart) <- isBold = checkBold rightPart dataText
    | Just (_, rightPart) <- isCode = checkCode rightPart dataText
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
