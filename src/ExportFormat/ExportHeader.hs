{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportHeader
-}

module ExportFormat.ExportHeader (exportHeader) where
import Content (PHeader(..))
import ExportFormat.ExportFormatData (ExportFormat(..))

exportHeaderJsonMaybeString :: String -> Maybe String -> String
exportHeaderJsonMaybeString field_name (Just str) = ",\n        \""
    ++ field_name ++ "\": \"" ++ str ++ "\""
exportHeaderJsonMaybeString _ Nothing = []

exportHeaderJson :: PHeader -> String
exportHeaderJson (PHeader title_ author_ date_) =
    "{\n    \"header\": {\n        \"title\": \"" ++ title_ ++ "\""
    ++ (exportHeaderJsonMaybeString "author" author_)
    ++ (exportHeaderJsonMaybeString "date" date_)
    ++ "\n    },\n"

exportHeaderXMLMaybeString :: String -> Maybe String -> String
exportHeaderXMLMaybeString field_name (Just str) = "\n        <"
    ++ field_name ++ ">" ++ str ++ "</" ++ field_name ++ ">"
exportHeaderXMLMaybeString _ Nothing = []

exportHeaderXML :: PHeader -> String
exportHeaderXML (PHeader title_ author_ date_) =
    "<document>\n    <header title=\"" ++ title_ ++ "\">"
    ++ (exportHeaderXMLMaybeString "author" author_)
    ++ (exportHeaderXMLMaybeString "date" date_)
    ++ "\n    </header>\n"

exportHeaderMDMaybeString :: String -> Maybe String -> String
exportHeaderMDMaybeString field_name (Just str) =
    "\n" ++ field_name ++ ": " ++ str
exportHeaderMDMaybeString _ Nothing = []

exportHeaderMD :: PHeader -> String
exportHeaderMD (PHeader title_ author_ date_) = "---\ntitle: " ++ title_
    ++ (exportHeaderMDMaybeString "author" author_)
    ++ (exportHeaderMDMaybeString "date" date_)
    ++ "\n"

exportHeader :: PHeader -> ExportFormat -> String
exportHeader header JSON = exportHeaderJson header
exportHeader header XML = exportHeaderXML header
exportHeader header MD = exportHeaderMD header
