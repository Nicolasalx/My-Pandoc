{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportBody
-}

module ExportFormat.ExportBody (exportBody) where
import Content (PBody(..))
import ExportFormat.ExportContent (exportContent)
import ExportFormat.ExportFormatData (ExportData(..))
import ExportFormat.MapExport (mapExport)

exportBody :: PBody -> ExportData -> String
exportBody (PBody (list)) exportData = "" -- start body ! to code
    ++ mapExport (`exportContent` exportData) "" list
    ++ "" -- end body ! to code
