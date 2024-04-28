{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseJsonKey
-}

module ParsingLib.ParseJsonKey (parseJsonKey) where
import ParsingLib.Strcmp (strcmp)

type Parser a = String -> Maybe (a , String)

parseJsonKey :: [String] -> Int -> Parser String
parseJsonKey [] _ _ = Nothing
parseJsonKey (x:xs) 2 input
    | strcmp x input = parseJsonKey xs 1 input
    | otherwise = Nothing
parseJsonKey (x:xs) 1 input
    | ':' `elem` x = parseJsonKey xs 0 input
    | otherwise    = Nothing
parseJsonKey (x:_) 0 input
    | not (null x) = Just (input, x)
    | otherwise    = Nothing
parseJsonKey _ _ _ = Nothing
