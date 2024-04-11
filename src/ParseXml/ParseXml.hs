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

parseXml :: String -> IO (Either String (PHeader, PBody))
parseXml file_content = do
    let headerResult = do
            title <- parseDocumentTitle file_content
            return title
    case headerResult of
        Just header -> return $ Right (header, PBody [])
        Nothing -> return $ Left "Erreur: thibaud pas content"

parseDocumentTitle :: String -> Maybe PHeader
parseDocumentTitle input = do
    (_, rest1) <- parseString "<title>" input
    (title, rest2) <- parseUntil '>' rest1
    return $ PHeader title Nothing Nothing

parseUntil :: Char -> String -> Maybe (String, String)
parseUntil c "" = Nothing
parseUntil c (x:xs)
    | x == c = Just ("", xs)
    | otherwise = do
        (parsed, rest) <- parseUntil c xs
        return (x:parsed, rest)
