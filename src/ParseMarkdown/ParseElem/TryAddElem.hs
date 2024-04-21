--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- TryAddElem
--

module ParseMarkdown.ParseElem.TryAddElem (tryAddElemToContent, fillElemEmptyActualList) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..))
import ParseMarkdown.FormatText.CreateText (createText)
import ParseMarkdown.ParseElem.Paragraph (createParagraph)

tryAddElemToContent :: DataParsing -> [PContent] -> IO ([PContent], DataParsing)
tryAddElemToContent dataParsing allContent
    -- | insertItem dataParsing = do
        -- let newData = dataParsing { insertItem = False }
        -- print (actualList newData)
        -- (createParagraph newData allContent)
        -- set insertItem Boolean to False
        -- create Paragraph and Push in the item
        -- Insérer le paragraphe dans le dernier (Item listItem) avec le (levelItem dataParsing)
    | typeToAdd dataParsing == Paragraph && insertLinkOrImage dataParsing && (length (actualList dataParsing)) > 0 = do
        newDataParsed <- createText (dataParsing { insertLinkOrImage = False })
        (createParagraph newDataParsed allContent)
    | typeToAdd dataParsing == Paragraph && insertLinkOrImage dataParsing = do
        (createParagraph (dataParsing { insertLinkOrImage = False }) allContent)
    | typeToAdd dataParsing == Paragraph && (nbReturnLines dataParsing) > 0 && (length (actualList dataParsing)) > 0 = do
        newDataParsed <- createText dataParsing
        (createParagraph newDataParsed allContent)
    | otherwise = return (allContent, dataParsing)

fillElemEmptyActualList :: DataParsing -> IO DataParsing
fillElemEmptyActualList dataParsing
    | hasFillCodeBlock dataParsing == True = return dataParsing { actualCodeBlock = actualCodeBlock dataParsing ++ [(actualList dataParsing)], actualList = "" }
    | isInCodeblock dataParsing == True = return dataParsing { hasFillCodeBlock = True }
    -- Implement new line for item
    | otherwise = return dataParsing
