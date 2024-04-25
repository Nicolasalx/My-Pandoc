{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseJson
-}

module ParseJson.ParseJson (parseJson) where
import Content (PHeader(..), PBody(..), PContent(..))
import ParseJson.ParseHeader(parseHeader)
import ParseJson.ParseBody(parseBody)

parseJsonBodyHelper :: Either String [PContent] -> PHeader -> Either String (PHeader, PBody)
parseJsonBodyHelper (Left err) _ = Left err
parseJsonBodyHelper (Right pContent) pHeader = Right (pHeader, PBody pContent)

parseJsonBody :: String -> PHeader -> Either String (PHeader, PBody)
parseJsonBody file_content pHeader =
    parseJsonBodyHelper (parseBody file_content) pHeader

parseJsonHelper :: Either String PHeader -> String -> Either String (PHeader, PBody)
parseJsonHelper (Left err) _ = Left err
parseJsonHelper (Right pHeader) file_content = parseJsonBody file_content pHeader

parseJson :: String -> Either String (PHeader, PBody)
parseJson file_content = parseJsonHelper (parseHeader file_content) file_content

