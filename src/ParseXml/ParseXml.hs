{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Lib (parseString)
import Content (PHeader(..), PBody(..))
import Data.Maybe (fromMaybe)
import ParseXml.DataStructXml (initializeDataParsing)

parseXml :: String -> IO (Either String (PHeader, PBody))
parseXml file_content = do
    let dataInitialized = initializeDataParsing
    let linesContent = lines file_content
    let headerResult = fillPHeader linesContent
    case headerResult of
        Right header -> return $ Right (header, PBody [])
        Left err -> return $ Left err

fillPHeader :: [String] -> Either String PHeader
fillPHeader [] = Right PHeader { header_title = "", author = Nothing, date = Nothing }
fillPHeader (x:xs)
    | Just ("<title>", value) <- parseString "<title>" x,
      Just (title, _) <- parseUntil '<' value,
      Right header <- fillPHeader xs =
          Right header { header_title = title }
    | Just ("<author>", value) <- parseString "<author>" x,
      Just (author, _) <- parseUntil '<' value,
      Right header <- fillPHeader xs =
          Right header { author = Just author }
    | Just ("<date>", value) <- parseString "<date>" x,
      Just (date, _) <- parseUntil '<' value,
      Right header <- fillPHeader xs =
          Right header { date = Just date }
    | elem '<' x = Right PHeader { header_title = "", author = Nothing, date = Nothing }
    | otherwise = Left "Erreur : Format d'en-tête invalide ou champ invalide"

parseUntil :: Char -> String -> Maybe (String, String)
parseUntil c "" = Nothing
parseUntil c (x:xs)
    | x == c = Just ("", xs)
    | otherwise = do
        (parsed, rest) <- parseUntil c xs
        return (x:parsed, rest)
