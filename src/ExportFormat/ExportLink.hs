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
exportLink (PLink url content_) JSON indent =
    addIndent (indent) ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"link\": {\n"
    ++ addIndent (indent + 2) ++ "\"url\": \"" ++ url ++ "\",\n"
    ++ addIndent (indent + 2) ++ "\"content\": [\n"
    ++ exportText content_ JSON (indent + 3) False ++ "\n"
    ++ addIndent (indent + 2) ++ "]\n"
    ++ addIndent (indent + 1) ++ "}\n"
    ++ addIndent (indent) ++ "}"

exportLink (PLink url content_) XML _ =
    "<link url=\"" ++ url ++ "\">"
        ++ exportText content_ XML 0 False ++ "</link>"

exportLink (PLink url content_) MD _ =
    "[" ++ exportText content_ MD 0 False ++ "](" ++ url ++ ")"
