{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- searchSymbol
-}

module ParsingLib.SearchSymbol (searchSymbol) where

searchSymbol :: String -> String -> Bool
searchSymbol [] _ = True
searchSymbol _ [] = False
searchSymbol (x:xs) (y:ys)
    | x == y = searchSymbol xs ys
    | otherwise = searchSymbol (x:xs) ys