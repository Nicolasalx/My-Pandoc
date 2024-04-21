{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportList
-}

module ExportFormat.ExportList (exportList) where
import Content (PList(..), PItem(..), PItemType(..))
import ExportFormat.AddIndent (addIndent)
import ExportFormat.ExportFormatData (ExportFormat(..), ExportData(..))
import ExportFormat.ExportParagraph (exportParagraph)
import ExportFormat.MapExport (mapExport)

exportItemType :: PItemType -> ExportData -> String
exportItemType (PParagraphItem paragraph) exportData =
    exportParagraph paragraph exportData
exportItemType (PListItem list) exportData = exportList list exportData

exportItem :: PItem -> ExportFormat -> ExportData -> String
exportItem (PItem list) JSON exportData =
    mapExport (\line -> exportItemType line exportData) ",\n" list
exportItem (PItem list) XML exportData =
    concatMap (\line -> exportItemType line exportData) list
exportItem (PItem list) MD exportData =
    concatMap (\line -> replicate ((list_level exportData) * 4) ' '
    ++ "- " ++ exportItemType line
        (exportData {list_level = (list_level exportData) + 1})) list

exportListHelper :: PList -> ExportFormat -> ExportData -> String
exportListHelper (PList list) JSON exportData =
    addIndent (indent_ exportData) ++ "{\n"
    ++ addIndent ((indent_ exportData) + 1) ++ "\"list\": [\n"
    ++ mapExport (\line -> exportItem line (format_ exportData)
        (exportData {indent_ = (indent_ exportData) + 2})) ",\n" list
    ++ addIndent ((indent_ exportData) + 1) ++ "]\n"
    ++ addIndent (indent_ exportData) ++ "}"
exportListHelper (PList list) XML exportData =
    addIndent (indent_ exportData) ++ "<list>\n"
    ++ concatMap (\line -> exportItem line (format_ exportData)
        (exportData {indent_ = (indent_ exportData) + 1})) list
    ++ addIndent (indent_ exportData) ++ "</list>\n"
exportListHelper (PList list) MD exportData =
    mapExport (\line -> exportItem line
        (format_ exportData) (exportData)) "\n" list

exportList :: PList -> ExportData -> String
exportList list exportData =
    exportListHelper list (format_ exportData) exportData
