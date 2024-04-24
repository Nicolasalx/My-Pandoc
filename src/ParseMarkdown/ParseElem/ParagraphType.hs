{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParagraphType
-}

module ParseMarkdown.ParseElem.ParagraphType (defineParagraphType,
    initNewSection, tryAddFrstSection) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..), TypeToAdd(..))
import Content (PContent(..), PSection(..))
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)

initNewSection :: String -> PContent
initNewSection titleSection = PSectionContent $ PSection
    { title = titleSection, section_content = [] }

tryAddFrstSection :: Int -> PContent -> [PContent] -> [PContent]
tryAddFrstSection 1 content allContent = addNewElemToContent content allContent
tryAddFrstSection _ _ allContent =
    addNewElemToContent (initNewSection "") allContent

defineParagraphType :: DataParsing -> String ->
    [PContent] -> (DataParsing, String, [PContent])
defineParagraphType dataParsing str allContent
    | not (isInCodeblock dataParsing) =
        (dataParsing { typeToAdd = Paragraph }, str, allContent)
    | otherwise = (dataParsing, str, allContent)
