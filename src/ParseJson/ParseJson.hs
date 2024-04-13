{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseJson
-}

module ParseJson.ParseJson (parseJson) where
import Content (PHeader(..), PBody(..))
import ParseJson.ParseHeader(parseHeader)
import ParseJson.ParseBody(parseBody)

parseJsonBody :: String -> PHeader -> IO (Either String (PHeader, PBody))
parseJsonBody file_content pHeader = do
    contentResult <- parseBody file_content
    case contentResult of
        Right pContent -> return $ Right (pHeader, PBody pContent)
        Left err -> return $ Left err

parseJson :: String -> IO (Either String (PHeader, PBody))
parseJson file_content = do
    headerResult <- parseHeader file_content
    case headerResult of
        Right pHeader -> parseJsonBody file_content pHeader
        Left err -> return $ Left err
