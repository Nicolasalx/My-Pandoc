{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportCodeBlock
-}

module ExportFormat.ExportCodeBlock (exportCodeBlock) where
import Content (PCodeBlock(..))
import ExportFormat.AddIndent (addIndent)
import ExportFormat.ExportFormatData (ExportFormat(..), ExportData(..))
import ExportFormat.MapExport (mapExport)

exportCodeLine :: String -> ExportFormat -> Int -> String
exportCodeLine code JSON indent = addIndent (indent + 1) ++ "[\n"
    ++ addIndent (indent + 2) ++ "\"" ++ code ++ "\"\n"
    ++ addIndent (indent + 1) ++ "]"
exportCodeLine code XML indent =
    addIndent (indent + 1) ++ "<paragraph>" ++ code ++ "</paragraph>"
exportCodeLine code MD _ = code

exportCodeBlock :: PCodeBlock -> ExportData -> String
exportCodeBlock (PCodeBlock (code)) (exportData) =
    (addIndent (indent_ exportData))
    ++ (start_codeblock exportData)
    ++ mapExport (\line -> exportCodeLine line (format_ exportData)
        (indent_ exportData)) (sep_codeblock exportData) code
    ++ "\n" ++ (addIndent (indent_ exportData))
    ++ (end_codeblock exportData)
