{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseHeader
-}

module ParseXmlNew.ParseHeader (parseHeader) where
import Content (PHeader(..))

parseHeader :: String -> Either String (PHeader, String)
parseHeader header_content = Right ((PHeader "" (Just "") (Just "")), "")
