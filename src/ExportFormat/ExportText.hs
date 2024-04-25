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

addTextIndent :: Int -> Bool -> String
addTextIndent indent False = addIndent indent
addTextIndent _ True = ""

exportBold :: PBold -> ExportFormat -> Int -> Bool -> String
exportBold (PBold text) JSON indent nested =
    addTextIndent indent nested ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"bold\": "
    ++ exportText (PText text) JSON (indent + 1) True
    ++ "\n" ++ addIndent indent ++ "}"
exportBold (PBold text) XML _ _ =
    "<bold>" ++ exportText (PText text) XML 0 False ++ "</bold>"
exportBold (PBold text) MD _ _ = "**"
    ++ exportText (PText text) MD 0 False ++ "**"

exportItalic :: PItalic -> ExportFormat -> Int -> Bool -> String
exportItalic (PItalic text) JSON indent nested =
    addTextIndent indent nested ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"italic\": "
    ++ exportText (PText text) JSON (indent + 1) True
    ++ "\n" ++ addIndent indent ++ "}"
exportItalic (PItalic text) XML _ _ =
    "<italic>" ++ exportText (PText text) XML 0 False ++ "</italic>"
exportItalic (PItalic text) MD _ _ = "*"
    ++ exportText (PText text) MD 0 False ++ "*"

exportCode :: PCode -> ExportFormat -> Int -> Bool -> String
exportCode (PCode text) JSON indent nested =
    addTextIndent indent nested ++ "{\n"
    ++ addIndent (indent + 1) ++ "\"code\": "
    ++ exportText (PText text) JSON (indent + 1) True
    ++ "\n" ++ addIndent indent ++ "}"
exportCode (PCode text) XML _ _ =
    "<code>" ++ exportText (PText text) XML 0 False ++ "</code>"
exportCode (PCode text) MD _ _ = "`"
    ++ exportText (PText text) MD 0 False ++ "`"

exportString :: String -> ExportFormat -> Int -> Bool -> String
exportString str JSON indent nested =
    (addTextIndent indent nested) ++ "\"" ++ str ++ "\""
exportString str XML _ _ = str
exportString str MD _ _ = str

exportTextType :: PTextType -> ExportFormat -> Int -> Bool -> String
exportTextType (PString str) format indent nested =
    exportString str format indent nested
exportTextType (PBoldText bold) format indent nested =
    exportBold bold format indent nested
exportTextType (PItalicText italic) format indent nested =
    exportItalic italic format indent nested
exportTextType (PCodeText code) format indent nested =
    exportCode code format indent nested

exportText :: PText -> ExportFormat -> Int -> Bool -> String
exportText (PText list) JSON indent nested =
    mapExport (\line -> exportTextType line JSON indent nested)
    ",\n" list
exportText (PText list) format indent _ =
    concatMap (\line -> exportTextType line format indent False) list
