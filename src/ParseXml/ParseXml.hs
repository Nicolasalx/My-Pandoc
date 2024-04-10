{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Content (PContent(..))

parseXml :: String -> Either String [PContent]
parseXml file = Right []
