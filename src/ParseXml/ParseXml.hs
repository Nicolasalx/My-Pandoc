{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Content (PHeader(..), PBody(..))
import ParseXml.ParseBody(parseBody)
import ParseXml.ParseHeader(fillPHeader)

parseXmlBody :: String -> PHeader -> Either String (PHeader, PBody)
parseXmlBody file_content pHeader = do
    case (parseBody file_content) of
        Right pContent -> Right (pHeader, PBody pContent)
        Left err -> Left err

parseXml :: String -> Either String (PHeader, PBody)
parseXml file_content = do
    case (fillPHeader (lines file_content)) of
        Right header -> do
            case (parseXmlBody file_content header) of
                Right (header_, body) -> Right (header_, body)
                Left err -> Left err
        Left err -> Left err
