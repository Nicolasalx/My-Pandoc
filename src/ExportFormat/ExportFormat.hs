{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportFormat
-}

module ExportFormat.ExportFormat (exportFormat) where
import ArgParsing (Format(..))
import Content (PHeader(..), PBody(..))
import ExportFormat.ExportBody (exportBody)
import ExportFormat.ExportFormatData (ExportFormat(..), initExportData)
import ExportFormat.ExportHeader (exportHeader)

getExportFormat :: Format -> ExportFormat
getExportFormat ArgParsing.JSON = ExportFormat.ExportFormatData.JSON
getExportFormat ArgParsing.XML = ExportFormat.ExportFormatData.XML
getExportFormat ArgParsing.MarkDown = ExportFormat.ExportFormatData.MD
getExportFormat ArgParsing.NotProvided = ExportFormat.ExportFormatData.JSON

exportFormat :: (PHeader, PBody) -> Format -> String
exportFormat (header, body) format =
    exportHeader header (getExportFormat format)
    ++ exportBody body (initExportData (getExportFormat format))
