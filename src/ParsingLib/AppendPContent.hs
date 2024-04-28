{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AppendPContent
-}

module ParsingLib.AppendPContent (appendPContent) where
import Content (PContent(..))
import ParsingLib.AddNewPContent (addNewPContent)

appendPContent :: [String] -> PContent -> [PContent] -> [PContent]
appendPContent state newC contenu = addNewPContent state False newC contenu
