--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Image
--

module ParseMarkdown.LinksAndImages.Image (insertImageToParagraph) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PParagraphType(..), PImage(..))
import ParseMarkdown.FormatText.CreateText (createText, formattingText)

insertImageToParagraph :: DataParsing -> IO DataParsing
insertImageToParagraph dataParsing
    | length (actualList dataParsing) > 0 = do
        newDataParsed <- createText dataParsing
        (insertImage newDataParsed)
    | otherwise = (insertImage dataParsing)

insertImage:: DataParsing -> IO DataParsing
insertImage dataParsing = do
    newImage <- formattingImg dataParsing
    return (dataParsing { paragraph = (paragraph dataParsing) ++ [newImage], urlImg = "", altImg = "", actualList = "" })

formattingImg :: DataParsing -> IO PParagraphType
formattingImg dataParsing = do
    textFormatted <- formattingText (altImg dataParsing)
    return (PImageParagraph (PImage (urlImg dataParsing) textFormatted))
