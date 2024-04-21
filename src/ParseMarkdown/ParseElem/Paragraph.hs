--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Paragraph
--

module ParseMarkdown.ParseElem.Paragraph (tryAddParagraph, createParagraph) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..), PParagraphType(..), PParagraph(..))
import ParseMarkdown.ParseElem.InsertInSection (checkInsertSection)

createParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
createParagraph dataParsing allContent
    | length (paragraph dataParsing) > 0 = do
        let newContent = convertParagraphToContent (paragraph dataParsing)
            finalContent = checkInsertSection dataParsing newContent allContent
        (finalContent, dataParsing { paragraph = [], actualList = "" })
    | otherwise = (allContent, dataParsing)

tryAddParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddParagraph dataParsing allContent
    | typeToAdd dataParsing == Paragraph = createParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

convertParagraphToContent :: [PParagraphType] -> PContent
convertParagraphToContent paragraphTypes = 
    PParagraphContent (PParagraph paragraphTypes)
