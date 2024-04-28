{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Strcmp
-}

module ParsingLib.Strcmp (strcmp) where

strcmp :: Eq a => [a] -> [a] -> Bool
strcmp [] _ = True
strcmp _ [] = False
strcmp (x:xs) (y:ys)
    | x == y = strcmp xs ys
    | otherwise = False
