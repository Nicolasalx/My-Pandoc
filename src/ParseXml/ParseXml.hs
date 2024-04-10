{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Content (PHeader(..), PBody(..))

parseXml :: String -> IO (Either String (PHeader, PBody))
parseXml file = return (Right ((PHeader "" (Just "") (Just "")), PBody []))
