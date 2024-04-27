{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseString
-}

module ParsingLib.ParseString (parseString) where

type Parser a = String -> Maybe (a , String)

parseString :: String -> Parser String
parseString [] input = Just ([], input)
parseString (s:str) (x:xs)
    | s == x, Just (parsed, rest) <- parseString str xs = Just (s:parsed, rest)
    | otherwise = Nothing
parseString _ _ = Nothing