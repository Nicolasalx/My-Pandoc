{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Content (PHeader(..), PBody(..), PContent(..))
import ParseXml.ParseBody(parseBody)
import ParseXml.ParseHeader(fillPHeader)

parseXmlBodyHelper :: Either String [PContent] -> PHeader
    -> Either String (PHeader, PBody)
parseXmlBodyHelper (Left err) _ = Left err
parseXmlBodyHelper (Right pContent) pHeader = Right (pHeader, PBody pContent)

parseXmlBody :: String -> PHeader -> Either String (PHeader, PBody)
parseXmlBody file_content pHeader =
    parseXmlBodyHelper (parseBody file_content) pHeader

parseXmlHelper :: String -> Either String PHeader -> Either String (PHeader, PBody)
parseXmlHelper _ (Left err) = Left err
parseXmlHelper file_content (Right header) = parseXmlBody file_content header

parseXml :: String -> Either String (PHeader, PBody)
parseXml file_content =
    parseXmlHelper file_content (fillPHeader (lines file_content))
