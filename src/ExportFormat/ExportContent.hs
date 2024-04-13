{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportContent
-}

module ExportFormat.ExportContent (exportContent) where
import Content (PContent(..))
import ExportFormat.ExportCodeBlock (exportCodeBlock)
import ExportFormat.ExportFormatData (ExportData(..))
import ExportFormat.ExportParagraph (exportParagraph)

exportContent :: PContent -> ExportData -> String
exportContent (PParagraphContent (paragraph)) exportData = exportParagraph paragraph exportData
exportContent (PSectionContent (section)) (exportData) = ""
exportContent (PCodeBlockContent (code_block)) (exportData) = exportCodeBlock code_block exportData
exportContent (PListContent (list)) (exportData) = ""
