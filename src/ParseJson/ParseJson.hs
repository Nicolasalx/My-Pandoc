{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseJson
-}

module ParseJson.ParseJson (parseJson) where

parseJson :: String -> IO () -- return value to be define
parseJson file = putStrLn ("parse Json:\n" ++ file)
