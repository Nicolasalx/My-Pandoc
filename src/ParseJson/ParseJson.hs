{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseJson
-}

module ParseJson.ParseJson (parseJson) where
import Content (PHeader(..), PBody(..))

parseJson :: String -> Either String (PHeader, PBody)
parseJson file = Right ((PHeader "" (Just "") (Just "")), PBody [])
