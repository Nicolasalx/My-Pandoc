{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Content (PHeader(..), PBody(..), PContent(..))
import ParseXml.ParseBody(parseBody)
import ParseXml.ParseHeader(parseHeader)

parseXmlBodyHelper :: Either String [PContent] -> PHeader -> Either String (PHeader, PBody)
parseXmlBodyHelper (Left err) _ = Left err
parseXmlBodyHelper (Right pContent) pHeader = Right (pHeader, PBody pContent)

parseXmlBody :: String -> PHeader -> Either String (PHeader, PBody)
parseXmlBody file_content pHeader =
    parseXmlBodyHelper (parseBody file_content) pHeader

parseXmlHelper :: Either String PHeader -> String -> Either String (PHeader, PBody)
parseXmlHelper (Left err) _ = Left err
parseXmlHelper (Right pHeader) file_content =
    parseXmlBody file_content pHeader

parseXml :: String -> Either String (PHeader, PBody)
parseXml file_content =
    parseXmlHelper (parseHeader file_content) file_content

