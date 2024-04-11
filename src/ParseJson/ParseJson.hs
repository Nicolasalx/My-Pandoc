{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseJson
-}

module ParseJson.ParseJson (parseJson) where
import Content (PHeader(..), PBody(..))
import ParseJson.ParseHeader(parseJsonHeader)

parseJson :: String -> IO (Either String (PHeader, PBody))
parseJson file_content = do
    let allLines = lines file_content
    headerResult <- parseJsonHeader allLines
    case headerResult of
        Right pHeader -> return $ Right (pHeader, PBody [])
        Left err -> return $ Left err
