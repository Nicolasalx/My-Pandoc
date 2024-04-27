{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DetectSymbol
-}

module ParseMarkdown.FormatText.DetectSymbol (detectSymbol) where
import ParseMarkdown.DataStructMarkdown (DataText(..))
import Content ()
import ParseMarkdown.FormatText.TextType.Bold (
    symbolBoldAlreadyOpen,
    symbolBoldNotOpen)
import ParseMarkdown.FormatText.TextType.Italic (
    symbolItalicAlreadyOpen,
    symbolItalicNotOpen)
import ParseMarkdown.FormatText.TextType.Code (
    symbolCodedAlreadyOpen,
    symbolCodeNotOpen)
import ParseMarkdown.FormatText.TryAddBasicList (parseBasicChar)
import ParsingLib.ParseString (parseString)

checkBold :: String -> DataText -> (String, DataText)
checkBold str dataText
    | isInBold dataText = symbolBoldAlreadyOpen str dataText
    | otherwise = symbolBoldNotOpen str dataText

checkItalic :: String -> DataText -> (String, DataText)
checkItalic str dataText
    | isInItalic dataText = symbolItalicAlreadyOpen str dataText
    | otherwise = symbolItalicNotOpen str dataText

checkCode :: String -> DataText -> (String, DataText)
checkCode str dataText
    | isInCode dataText = symbolCodedAlreadyOpen str dataText
    | otherwise = symbolCodeNotOpen str dataText

detectSymbol :: String -> DataText -> (String, DataText)
detectSymbol str dataText
    | Just (_, rightPart) <- isBold, not (isInItalic dataText) =
        checkBold rightPart dataText
    | Just (_, rightPart) <- isItalic = checkItalic rightPart dataText
    | Just (_, rightPart) <- isBold = checkBold rightPart dataText
    | otherwise = detectSymbolOpt str dataText
    where
        isBold = parseString "**" str
        isItalic = parseString "*" str

detectSymbolOpt :: String -> DataText -> (String, DataText)
detectSymbolOpt str dataText
    | Just (_, rightPart) <- isCode = checkCode rightPart dataText
    | not (null str) = (str, parseBasicChar dataText (head str))
    | otherwise = (str, dataText)
    where
        isCode = parseString "`" str
