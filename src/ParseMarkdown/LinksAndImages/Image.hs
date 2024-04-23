{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Image
-}

module ParseMarkdown.LinksAndImages.Image (insertImageToParagraph) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import Content (PParagraphType(..), PImage(..))
import ParseMarkdown.FormatText.CreateText (createText, formattingText)

insertImageToParagraph :: DataParsing -> DataParsing
insertImageToParagraph dataParsing
    | length (actualList dataParsing) > 0 =
        insertImage (createText (dataParsing { insertLinkOrImage = True }))
    | otherwise = insertImage dataParsing

insertImage :: DataParsing -> DataParsing
insertImage dataParsing =
    dataParsing { paragraph = (paragraph dataParsing) ++
    [formattingImg dataParsing], urlImg = "", altImg = "", actualList = "" }

formattingImg :: DataParsing -> PParagraphType
formattingImg dataParsing =
    PImageParagraph (PImage (urlImg dataParsing)
        (formattingText (altImg dataParsing)))
