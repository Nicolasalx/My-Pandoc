{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- CheckIsInString
-}

module ParsingLib.CheckIsInString (checkIsInString) where

checkIsInString :: String -> String -> Bool
checkIsInString [] _ = False
checkIsInString _ [] = False
checkIsInString (x:xs) str
    | x `elem` str = True
    | otherwise = checkIsInString xs str
