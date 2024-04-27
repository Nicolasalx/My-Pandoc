{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- LastPContent
-}

module ParsingLib.LastPContent (lastPContent) where
import Content (PContent(..))
import ParsingLib.GetLastPContent (getLastPContent)

lastPContent :: [String] -> [PContent] -> PContent
lastPContent state contenu = getLastPContent state False contenu