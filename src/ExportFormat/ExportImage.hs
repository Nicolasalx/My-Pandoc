{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportImage
-}

module ExportFormat.ExportImage (exportImage) where
import Content (PImage(..))
import ExportFormat.AddIndent (addIndent)
import ExportFormat.ExportFormatData (ExportFormat(..))
import ExportFormat.ExportText (exportText)

exportImage :: PImage -> ExportFormat -> Int -> String
exportImage (PImage url alt_) JSON indent =
    addIndent (indent) ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"image\": {\n"
    ++ addIndent (indent + 2) ++ "\"url\": \"" ++ url ++ "\",\n"
    ++ addIndent (indent + 2) ++ "\"alt\": [\n"
    ++ exportText alt_ JSON (indent + 3) False ++ "\n"
    ++ addIndent (indent + 2) ++ "]\n"
    ++ addIndent (indent + 1) ++ "}\n"
    ++ addIndent (indent) ++ "}"

exportImage (PImage url alt_) XML _ =
    "<image url=\"" ++ url ++ "\">"
        ++ exportText alt_ XML 0 False ++ "</image>"

exportImage (PImage url alt_) MD _ =
    "![" ++ exportText alt_ MD 0 False ++ "](" ++ url ++ ")"
