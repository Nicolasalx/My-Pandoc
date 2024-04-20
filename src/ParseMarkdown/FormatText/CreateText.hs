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

createText :: DataParsing -> IO DataParsing
createText dataParsing
    | length (actualList dataParsing) > 0 = do
        newTextType <- formattingElemParagraph dataParsing
        return dataParsing { paragraph = (paragraph dataParsing) ++ [newTextType] }
    | otherwise = return dataParsing

formattingElemParagraph :: DataParsing -> IO PParagraphType
formattingElemParagraph dataParsing = do
    textFormatted <- formattingText (actualList dataParsing)
    return (PTextParagraph textFormatted)

formattingText :: String -> IO PText
formattingText str = do
    let dataText = initializeDataText
    newDataText <- browseStr str dataText False
    let finalData = tryAddBasicToList newDataText
    newData <- closeAllDelim finalData
    newList <- formatLastList dataText (listText newData) []
    -- Concat all strings in list
    endData <- appendAllElem newData newList
    return (contentText endData)
