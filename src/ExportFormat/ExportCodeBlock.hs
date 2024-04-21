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
exportCodeLine code JSON indent = addIndent indent ++ "[\n"
    ++ addIndent (indent + 1) ++ "\"" ++ code ++ "\"\n"
    ++ addIndent indent ++ "]"
exportCodeLine code XML indent =
    addIndent indent ++ "<paragraph>" ++ code ++ "</paragraph>"
exportCodeLine code MD _ = code

exportCodeBlockHelper :: PCodeBlock -> ExportFormat -> Int -> String
exportCodeBlockHelper (PCodeBlock (code)) JSON indent =
    (addIndent indent) ++ "{\n" ++
    (addIndent (indent + 1)) ++ "\"codeblock\": [\n"
    ++ mapExport (\line -> exportCodeLine line JSON (indent + 2)) ",\n" code
    ++ "\n"
    ++ (addIndent (indent + 1)) ++ "]\n"
    ++ (addIndent indent) ++ "}"

exportCodeBlockHelper (PCodeBlock (code)) XML indent =
    (addIndent indent)
    ++ "<codeblock>\n"
    ++ mapExport (\line -> exportCodeLine line XML (indent + 1)) "\n" code
    ++ "\n" ++ (addIndent indent)
    ++ "</codeblock>\n"

exportCodeBlockHelper (PCodeBlock (code)) MD _ =
    "\n```\n"
    ++ mapExport (\line -> exportCodeLine line MD 0) "\n" code
    ++ "\n"
    ++ "```\n"

exportCodeBlock :: PCodeBlock -> ExportData -> String
exportCodeBlock codeBlock exportData =
    exportCodeBlockHelper codeBlock (format_ exportData) (indent_ exportData)