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
exportImage (PImage url alt) JSON indent =
    addIndent (indent) ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"image\": {\n"
    ++ addIndent (indent + 2) ++ "\"url\": \"" ++ url ++ "\",\n"
    ++ addIndent (indent + 2) ++ "\"alt\": [\n"
    ++ exportText alt JSON (indent + 3) ++ "\n"
    ++ addIndent (indent + 2) ++ "]\n"
    ++ addIndent (indent + 1) ++ "}\n"
    ++ addIndent (indent) ++ "}"

exportImage (PImage url alt) XML _ =
    "<image url=\"" ++ url ++ "\">" ++ exportText alt XML 0 ++ "</image>"

exportImage (PImage url alt) MD _ =
    "![" ++ exportText alt MD 0 ++ "](" ++ url ++ ")"
