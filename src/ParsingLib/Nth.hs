{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Nth
-}

module ParsingLib.Nth (nth) where

nth :: Int -> [String] -> [String]
nth _ [] = []
nth 0 x = x
nth n (_:xs) = nth (n-1) xs
