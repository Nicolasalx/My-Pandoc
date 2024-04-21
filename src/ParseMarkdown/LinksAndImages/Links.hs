--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Links
--

module ParseMarkdown.LinksAndImages.Links (insertLinkToParagraph) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PParagraphType(..), PLink(..))
import ParseMarkdown.FormatText.CreateText (createText, formattingText)

insertLinkToParagraph :: DataParsing -> DataParsing
insertLinkToParagraph dataParsing
    | length (actualList dataParsing) > 0 = do
        let newDataParsed = createText (dataParsing { insertLinkOrImage = True })
        (insertLink newDataParsed)
    | otherwise = (insertLink dataParsing)

insertLink :: DataParsing -> DataParsing
insertLink dataParsing = do
    let newLink = formattingLink dataParsing
    (dataParsing { paragraph = (paragraph dataParsing) ++ [newLink], urlLink = "", contentLink = "", actualList = "" })

formattingLink :: DataParsing -> PParagraphType
formattingLink dataParsing = do
    let textFormatted = formattingText (contentLink dataParsing)
    (PLinkParagraph (PLink (urlLink dataParsing) textFormatted))
