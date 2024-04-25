{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Content (PHeader(..), PBody(..))
import ParseXml.ParseHeader(fillPHeader)
import ParseXml.ParseBody(parseBody)

parseXmlBody :: String -> PHeader -> IO (Either String (PHeader, PBody))
parseXmlBody file_content pHeader = do
    bodyResult <- parseBody file_content
    case bodyResult of
        Right pContent -> return $ Right (pHeader, PBody pContent)
        Left err -> return $ Left err

parseXml :: String -> IO (Either String (PHeader, PBody))
parseXml file_content = do
    let linesContent = lines file_content
    let headerResult = fillPHeader linesContent
    case headerResult of
        Right header -> do
            bodyResult <- parseXmlBody file_content header
            case bodyResult of
                Right (header, body) -> return $ Right (header, body)
                Left err -> return $ Left err
        Left err -> return $ Left err

