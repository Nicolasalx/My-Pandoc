{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- StrToWordArray
-}

module ParsingLib.StrToWordArray (strToWordArray) where

strToWordArrayHelper :: String -> Char -> Bool
strToWordArrayHelper [] _ = True
strToWordArrayHelper (x:xs) c
    | x == c = False
    | otherwise = strToWordArrayHelper xs c

strToWordArray :: String -> String -> String -> [String]
strToWordArray _ [] [] = []
strToWordArray _ tmp [] = tmp : []
strToWordArray str tmp (x:xs)
    | strToWordArrayHelper str x = strToWordArray str (tmp ++ [x]) xs
    | length tmp == 0 = strToWordArray str [] xs
    | otherwise = tmp : strToWordArray str [] xs