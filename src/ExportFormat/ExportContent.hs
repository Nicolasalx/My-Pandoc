{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportContent
-}

module ExportFormat.ExportContent (exportContent) where
import Content (PContent(..), PSection(..))
import ExportFormat.AddIndent (addIndent)
import ExportFormat.ExportCodeBlock (exportCodeBlock)
import ExportFormat.ExportFormatData (ExportFormat(..), ExportData(..))
import ExportFormat.ExportList (exportList)
import ExportFormat.ExportParagraph (exportParagraph)
import ExportFormat.MapExport (mapExport)
import ExportFormat.AddLineBreak (addLineBreak)

exportSectionType :: PSection -> ExportFormat -> ExportData -> String
exportSectionType (PSection title_ content) JSON exportData =
    addIndent (indent_ exportData) ++ "{\n"
    ++ addIndent ((indent_ exportData) + 1) ++ "\"section\": {\n"
    ++ addIndent ((indent_ exportData) + 2) ++ "\"title\": \"" ++ title_
    ++ "\",\n" ++ addIndent ((indent_ exportData) + 2) ++ "\"content\": [\n"
    ++ addLineBreak (mapExport (\line -> exportContent line
        (exportData {indent_ = (indent_ exportData) + 3})) ",\n" content)
    ++ addIndent ((indent_ exportData) + 2) ++ "]\n"
    ++ addIndent ((indent_ exportData) + 1) ++ "}\n"
    ++ addIndent ((indent_ exportData)) ++ "}"

exportSectionType (PSection title_ content) XML exportData =
    addIndent (indent_ exportData) ++ "<section title=\"" ++ title_ ++ "\">\n"
    ++ concatMap (\line -> exportContent line (exportData {indent_ = (indent_ exportData) + 1})) content
    ++ addIndent (indent_ exportData) ++ "</section>\n"

exportSectionType (PSection title_ content) MD exportData
    | length title_ == 0 = concatMap (\line -> exportContent line
    (exportData {current_section = (current_section exportData) + 1})) content
    | otherwise = "\n" ++ replicate (current_section exportData) '#'
    ++ " " ++ title_ ++ "\n" ++
    concatMap (\line -> exportContent line
    (exportData {current_section = (current_section exportData) + 1})) content

exportSection :: PSection -> ExportData -> String
exportSection section exportData =
    exportSectionType section (format_ exportData) exportData

exportContent :: PContent -> ExportData -> String
exportContent (PParagraphContent (paragraph)) exportData =
    exportParagraph paragraph exportData
exportContent (PSectionContent (section)) (exportData) =
    exportSection section exportData
exportContent (PCodeBlockContent (code_block)) (exportData) =
    exportCodeBlock code_block exportData
exportContent (PListContent (list)) (exportData) = exportList list exportData
