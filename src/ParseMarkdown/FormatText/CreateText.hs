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
    | length (actualList dataParsing) > 0 = 
        dataParsing { paragraph = paragraph dataParsing ++ [formattingElemParagraph dataParsing] }
    | otherwise = dataParsing

formattingElemParagraph :: DataParsing -> PParagraphType
formattingElemParagraph dataParsing =
    PTextParagraph (formattingText (actualList dataParsing))

formattingText :: String -> PText
formattingText str =
    contentText $ appendAllElem (closeAllDelim (tryAddBasicToList (browseStr str initializeDataText False)))
                 (formatLastList initializeDataText (listText (closeAllDelim (tryAddBasicToList (browseStr str initializeDataText False)))) [])
