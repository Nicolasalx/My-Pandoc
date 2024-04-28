{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- InitPContent
-}

module ParsingLib.InitPContent (initPContent) where
import Content (PContent(..))
import ParsingLib.RmLastPContent (rmLastPContent)

initPContent :: [String] -> [PContent] -> [PContent]
initPContent state contenu = rmLastPContent state False contenu
