--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParagraphType
--

module ParseMarkdown.ParseElem.ParagraphType (defineParagraphType, initNewSection, tryAddFrstSection) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..), PSection(..))
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)

initNewSection :: String -> PContent
initNewSection titleSection = PSectionContent $ PSection { title = titleSection, section_content = [] }

tryAddFrstSection :: Int -> PContent -> [PContent] -> [PContent]
tryAddFrstSection levelSect content allContent
    | levelSect == 1 = addNewElemToContent content allContent
    | otherwise = addNewElemToContent (initNewSection "") allContent

defineParagraphType :: DataParsing -> String -> [PContent] -> IO (DataParsing, String, [PContent])
defineParagraphType dataParsing str allContent
    | not (isInCodeblock dataParsing) && not (isInParagraph dataParsing) = return (dataParsing { typeToAdd = Paragraph }, str, allContent)
    | otherwise = return (dataParsing, str, allContent)
