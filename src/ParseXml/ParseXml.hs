{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where

parseXml :: String -> IO () -- return value to be define
parseXml file = putStrLn ("parse xml:\n" ++ file)
