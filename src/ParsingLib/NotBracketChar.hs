{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- NotBracketChar
-}

module ParsingLib.NotBracketChar (notBracketChar) where

notBracketChar :: String -> Bool
notBracketChar [] = False
notBracketChar (',':_) = True
notBracketChar (' ':_) = True
notBracketChar ('\n':_) = True
notBracketChar (':':_) = True
notBracketChar (_:_) = False