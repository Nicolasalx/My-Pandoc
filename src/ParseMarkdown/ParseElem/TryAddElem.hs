{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- TryAddElem
-}

module ParseMarkdown.ParseElem.TryAddElem (tryAddElemToContent,
    fillElemEmptyActualList) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..))
import ParseMarkdown.FormatText.CreateText (createText)
import ParseMarkdown.ParseElem.Paragraph (createParagraph, insertListItem)

tryAddElemToContent :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddElemToContent dataParsing allContent
    | length (listItem dataParsing) > 0 && (nbReturnLines dataParsing) > 0 =
            insertListItem dataParsing allContent
    | typeToAdd dataParsing == Paragraph && insertLinkOrImage dataParsing
        && (length (actualList dataParsing)) > 0 =
        (createParagraph (createText (dataParsing
            { insertLinkOrImage = False })) allContent)
    | otherwise = tryAddRestToContent dataParsing allContent

tryAddRestToContent :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddRestToContent dataParsing allContent
    | typeToAdd dataParsing == Paragraph && insertLinkOrImage dataParsing =
        (createParagraph (dataParsing
            { insertLinkOrImage = False }) allContent)
    | typeToAdd dataParsing == Paragraph && (nbReturnLines dataParsing) > 0
        && (length (actualList dataParsing)) > 0 =
        (createParagraph (createText dataParsing) allContent)
    | otherwise = (allContent, dataParsing)

fillElemEmptyActualList :: DataParsing -> DataParsing
fillElemEmptyActualList dataParsing
    | hasFillCodeBlock dataParsing == True = dataParsing { actualCodeBlock =
        actualCodeBlock dataParsing ++ [(actualList dataParsing)],
        actualList = "" }
    | isInCodeblock dataParsing == True =
        dataParsing { hasFillCodeBlock = True }
    | otherwise = dataParsing
