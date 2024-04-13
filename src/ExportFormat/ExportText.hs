{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportText
-}

module ExportFormat.ExportText (exportText) where
import Content (PText(..), PTextType(..))
import ExportFormat.AddIndent (addIndent)
import ExportFormat.MapExport (mapExport)
import ExportFormat.ExportFormatData (ExportFormat(..))

exportTextType :: PTextType -> ExportFormat -> Int -> String
exportTextType text_type JSON indent = ""
exportTextType text_type XML indent = ""
exportTextType text_type MD indent = ""

exportText :: PText -> ExportFormat -> Int -> String
exportText (PText list) format indent =
    mapExport (\line -> exportTextType line format indent) "" list
