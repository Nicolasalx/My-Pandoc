{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Paragraph
-}

module ParseMarkdown.ParseElem.Paragraph (tryAddParagraph, createParagraph,
    insertListItem) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..), PParagraphType(..),
    PParagraph(..), PItem(..), PList(..))
import ParseMarkdown.ParseElem.InsertInSection (checkInsertSection)

createParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
createParagraph dataParsing allContent
    | length (paragraph dataParsing) > 0 =
        (checkInsertSection dataParsing (convertParagraphToContent
        (paragraph dataParsing)) allContent, dataParsing
        { paragraph = [], actualList = "" })
    | otherwise = (allContent, dataParsing)

tryAddParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddParagraph dataParsing allContent
    | length (listItem dataParsing) > 0 =
        insertListItem  dataParsing allContent
    | typeToAdd dataParsing == Paragraph =
        createParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

transformListToContent :: [PItem] -> PContent
transformListToContent items = PListContent (PList items)

insertListItem :: DataParsing -> [PContent] -> ([PContent], DataParsing)
insertListItem dataParsing allContent =
    (checkInsertSection dataParsing (transformListToContent
    (listItem dataParsing)) allContent, dataParsing { listItem = [] })

convertParagraphToContent :: [PParagraphType] -> PContent
convertParagraphToContent paragraphTypes = 
    PParagraphContent (PParagraph paragraphTypes)
