{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Links
-}

module ParseMarkdown.LinksAndImages.Links (insertLinkToParagraph) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PParagraphType(..), PLink(..))
import ParseMarkdown.FormatText.CreateText (createText, formattingText)

insertLinkToParagraph :: DataParsing -> DataParsing
insertLinkToParagraph dataParsing
    | length (actualList dataParsing) > 0 =
        insertLink (createText (dataParsing { insertLinkOrImage = True }))
    | otherwise = insertLink dataParsing

insertLink :: DataParsing -> DataParsing
insertLink dataParsing =
    dataParsing { paragraph = (paragraph dataParsing) ++
        [formattingLink dataParsing],
            urlLink = "", contentLink = "", actualList = "" }

formattingLink :: DataParsing -> PParagraphType
formattingLink dataParsing =
    PLinkParagraph (PLink (urlLink dataParsing)
        (formattingText (contentLink dataParsing)))
