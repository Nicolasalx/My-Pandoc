{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportParagraph
-}

module ExportFormat.ExportParagraph (exportParagraph) where
import Content (PParagraph(..), PParagraphType(..))
import ExportFormat.ExportFormatData (ExportData(..), ExportFormat(..))
import ExportFormat.ExportImage (exportImage)
import ExportFormat.ExportLink (exportLink)
import ExportFormat.ExportText (exportText)
import ExportFormat.MapExport (mapExport)
import ExportFormat.AddIndent

exportParagraphType :: PParagraphType -> ExportData -> String
exportParagraphType (PTextParagraph text) (exportData) =
    exportText text (format_ exportData) (indent_ exportData)
exportParagraphType (PLinkParagraph link) (exportData) =
    exportLink link (format_ exportData) (indent_ exportData)
exportParagraphType (PImageParagraph image) (exportData) =
    exportImage image (format_ exportData) (indent_ exportData)

exportParagraphHelper :: PParagraph -> ExportFormat -> ExportData -> String
exportParagraphHelper (PParagraph (list)) XML (exportData) =
    (addIndent (indent_ exportData)) ++ (start_paragraph exportData) ++
    mapExport (\line -> exportParagraphType line
        (exportData)) (sep_paragraph exportData) list
    ++ (end_paragraph exportData)

exportParagraphHelper (PParagraph (list)) _ (exportData) =
    (start_paragraph exportData) ++
    mapExport (\line -> exportParagraphType line
        (exportData)) (sep_paragraph exportData) list
    ++ (end_paragraph exportData)

exportParagraph :: PParagraph -> ExportData -> String
exportParagraph paragraph (exportData) =
    exportParagraphHelper paragraph (format_ exportData) exportData
