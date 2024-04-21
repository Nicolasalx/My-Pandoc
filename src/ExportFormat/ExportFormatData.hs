{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportFormatData
-}

module ExportFormat.ExportFormatData (
    ExportFormat(..), ExportData(..), initExportData) where

data ExportFormat = JSON | XML | MD
    deriving (Show)

data ExportData = ExportData {
    format_ :: ExportFormat,
    indent_ :: Int,
    current_section :: Int,
    list_level :: Int,
    end_document :: String
} deriving (Show)

initExportData :: ExportFormat -> ExportData
initExportData JSON = (ExportData JSON 1 0 0
    "}\n")

initExportData XML = (ExportData XML 1 0 0
    "</document>\n")

initExportData MD = (ExportData MD 0 1 0
    "")
