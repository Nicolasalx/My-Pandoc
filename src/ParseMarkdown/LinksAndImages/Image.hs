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

insertImageToParagraph :: DataParsing -> DataParsing
insertImageToParagraph dataParsing
    | length (actualList dataParsing) > 0 = do
        let newDataParsed = createText (dataParsing { insertLinkOrImage = True })
        (insertImage newDataParsed)
    | otherwise = (insertImage dataParsing)

insertImage:: DataParsing -> DataParsing
insertImage dataParsing = do
    let newImage = formattingImg dataParsing
    (dataParsing { paragraph = (paragraph dataParsing) ++ [newImage], urlImg = "", altImg = "", actualList = "" })

formattingImg :: DataParsing -> PParagraphType
formattingImg dataParsing = do
    let textFormatted = formattingText (altImg dataParsing)
    (PImageParagraph (PImage (urlImg dataParsing) textFormatted))
