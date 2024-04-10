{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseJson
-}

module ParseJson.ParseJson (parseJson) where
import Content (PHeader(..), PBody(..))
import Lib (parseString)

parseEachLine :: [String] -> PHeader -> Either String PHeader
parseEachLine [] x = Right x
parseEachLine (x:xs) pHeader
    | Just ("\"title\":", value) <- parseString "\"title\":" x =
        parseEachLine xs pHeader { header_title = value }
    | Just ("\"author\":", value) <- parseString "\"author\":" x =
        parseEachLine xs pHeader { author = Just value }
    | Just ("\"date\":", value) <- parseString "\"date\":" x =
        parseEachLine xs pHeader { date = Just value }
    | otherwise = Left "Error: Invalid header format or field"

parseJsonHeader :: [String] -> IO (Either String PHeader)
parseJsonHeader [] = return $ Left "Empty file"
parseJsonHeader (x:xs) = return $ parseEachLine (x:xs) PHeader { header_title = "", author = Nothing, date = Nothing }

parseJson :: String -> IO (Either String (PHeader, PBody))
parseJson file_content = do
    let allLines = lines file_content
    headerResult <- parseJsonHeader allLines
    case headerResult of
        Right pHeader -> return $ Right (pHeader, PBody [])
        Left err -> return $ Left err
