--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- createText
--

module ParseMarkdown.FormatText.CreateText (createText, formattingText) where
import Content (PParagraphType(..), PText(..))
import ParseMarkdown.DataStructMarkdown (DataParsing(..), initializeDataText, DataText(..))
import ParseMarkdown.FormatText.CleanList (closeAllDelim)
import ParseMarkdown.FormatText.BrowseStr (browseStr)
import ParseMarkdown.FormatText.FormatList (formatLastList)
import ParseMarkdown.FormatText.AppendElemToList (appendAllElem)
import ParseMarkdown.FormatText.TryAddBasicList (tryAddBasicToList)

createText :: DataParsing -> DataParsing
createText dataParsing
    | length (actualList dataParsing) > 0 = do
        let newTextType = formattingElemParagraph dataParsing
        dataParsing { paragraph = (paragraph dataParsing) ++ [newTextType] }
    | otherwise = dataParsing

formattingElemParagraph :: DataParsing -> PParagraphType
formattingElemParagraph dataParsing = do
    let textFormatted = formattingText (actualList dataParsing)
    (PTextParagraph textFormatted)

formattingText :: String -> PText
formattingText str = do
    let dataText = initializeDataText
        newDataText = browseStr str dataText False
        finalData = tryAddBasicToList newDataText
        newData = closeAllDelim finalData
        newList = formatLastList dataText (listText newData) []
        endData = appendAllElem newData newList
    (contentText endData)
