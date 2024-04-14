{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportText
-}

module ExportFormat.ExportText (exportText) where
import Content (PText(..), PTextType(..), PBold(..), PItalic(..), PCode(..))
import ExportFormat.AddIndent (addIndent)
import ExportFormat.ExportFormatData (ExportFormat(..))
import ExportFormat.MapExport (mapExport)

exportBold :: PBold -> ExportFormat -> Int -> String
exportBold (PBold text) JSON indent =
    addIndent (indent - 1) ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"bold\": "
    ++ exportText (PText text) JSON (indent + 1)
    ++ addIndent indent ++ "}"
exportBold (PBold text) XML _ = "<bold>" ++ exportText (PText text) XML 0 ++ "</bold>"
exportBold (PBold text) MD _ = "**" ++ exportText (PText text) MD 0 ++ "**"

exportItalic :: PItalic -> ExportFormat -> Int -> String
exportItalic (PItalic text) JSON indent =
    addIndent (indent - 1) ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"italic\": "
    ++ exportText (PText text) JSON (indent + 1)
    ++ addIndent indent ++ "}"
exportItalic (PItalic text) XML _ = "<italic>" ++ exportText (PText text) XML 0 ++ "</italic>"
exportItalic (PItalic text) MD _ = "*" ++ exportText (PText text) MD 0 ++ "*"

exportCode :: PCode -> ExportFormat -> Int -> String
exportCode (PCode text) JSON indent =
    addIndent (indent - 1) ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"code\": "
    ++ exportText (PText text) JSON (indent + 1)
    ++ addIndent indent ++ "}"
exportCode (PCode text) XML _ = "<code>" ++ exportText (PText text) XML 0 ++ "</code>"
exportCode (PCode text) MD _ = "`" ++ exportText (PText text) MD 0 ++ "`"

exportString :: String -> ExportFormat -> Int -> String
exportString str JSON _ = "\"" ++ str ++ "\""
exportString str XML _ = str
exportString str MD _ = str

exportTextType :: PTextType -> ExportFormat -> Int -> String
exportTextType (PString str) format indent = exportString str format indent
exportTextType (PBoldText bold) format indent = exportBold bold format indent
exportTextType (PItalicText italic) format indent = exportItalic italic format indent
exportTextType (PCodeText code) format indent = exportCode code format indent

exportText :: PText -> ExportFormat -> Int -> String
exportText (PText list) JSON indent =
    mapExport (\line -> (addIndent indent) ++ exportTextType line JSON indent) ",\n" list
    ++ "\n"
exportText (PText list) format indent =
    concatMap (\line -> exportTextType line format indent) list
