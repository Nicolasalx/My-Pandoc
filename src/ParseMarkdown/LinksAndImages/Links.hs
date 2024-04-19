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

insertLinkToParagraph :: DataParsing -> IO DataParsing
insertLinkToParagraph dataParsing
    | length (actualList dataParsing) > 0 = do
        newDataParsed <- createText dataParsing
        (insertLink newDataParsed)
    | otherwise = (insertLink dataParsing)

insertLink :: DataParsing -> IO DataParsing
insertLink dataParsing = do
    newLink <- formattingLink dataParsing
    return (dataParsing { paragraph = (paragraph dataParsing) ++ [newLink], urlLink = "", contentLink = "", actualList = "" })

formattingLink :: DataParsing -> IO PParagraphType
formattingLink dataParsing = do
    textFormatted <- formattingText (contentLink dataParsing)
    return (PLinkParagraph (PLink (urlLink dataParsing) textFormatted))
