{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Content (PHeader(..), PBody(..))

parseXml :: String -> Either String (PHeader, PBody)
parseXml file = Right ((PHeader "" (Just "") (Just "")), PBody [])
