{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- GetLastPContent
-}

module ParsingLib.GetLastPContent (getLastPContent) where
import Content (PContent(..))
import ParsingLib.GetLastPSection (getLastPSection)

getLastPContent :: [String] -> Bool -> [PContent] -> PContent
getLastPContent [] _ contenu = last contenu
getLastPContent ("section":xs) True contenu
    = getLastPContent (xs) True (getLastPSection (last contenu))
getLastPContent ("section":xs) False contenu
    = getLastPContent (xs) True contenu
getLastPContent (_:xs) isBody contenu = getLastPContent (xs) isBody contenu
