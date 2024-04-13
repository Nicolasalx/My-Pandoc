{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportLink
-}

module ExportFormat.ExportLink (exportLink) where
import Content (PLink(..))
import ExportFormat.AddIndent (addIndent)
import ExportFormat.ExportFormatData (ExportFormat(..))
import ExportFormat.ExportText (exportText)

exportLink :: PLink -> ExportFormat -> Int -> String
exportLink (PLink url content) JSON indent =
    addIndent (indent) ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"link\": {\n"
    ++ addIndent (indent + 2) ++ "\"url\": \"" ++ url ++ "\",\n"
    ++ addIndent (indent + 2) ++ "\"content\": [\n"
    ++ exportText content JSON (indent + 3) ++ "\n"
    ++ addIndent (indent + 2) ++ "]\n"
    ++ addIndent (indent + 1) ++ "}\n"
    ++ addIndent (indent) ++ "}"

exportLink (PLink url content) XML _ =
    "<link url=\"" ++ url ++ "\">" ++ exportText content XML 0 ++ "</link>"

exportLink (PLink url content) MD _ =
    "[" ++ exportText content MD 0 ++ "](" ++ url ++ ")"
