{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportBody
-}

module ExportFormat.ExportBody (exportBody) where
import Content (PBody(..))
import ExportFormat.AddIndent (addIndent)
import ExportFormat.ExportContent (exportContent)
import ExportFormat.ExportFormatData (ExportData(..), ExportFormat(..))

exportBodyHelper :: PBody -> ExportFormat -> ExportData -> String
exportBodyHelper (PBody list) JSON exportData =
    addIndent (indent_ exportData) ++ "\"body\": [\n"
    ++ concatMap (\line -> exportContent line (exportData {indent_ = (indent_ exportData) + 1})) list
    ++ addIndent (indent_ exportData) ++ "]\n"

exportBodyHelper (PBody list) XML exportData =
    addIndent (indent_ exportData) ++ "<body>\n"
    ++ concatMap (\line -> exportContent line (exportData {indent_ = (indent_ exportData) + 1})) list
    ++ addIndent (indent_ exportData) ++ "</body>\n"

exportBodyHelper (PBody list) MD exportData = "---\n"
    ++ concatMap (\line -> exportContent line exportData) list

exportBody :: PBody -> ExportData -> String
exportBody body exportData = exportBodyHelper body (format_ exportData) exportData
