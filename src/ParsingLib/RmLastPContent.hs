{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- RmLastPContent
-}

module ParsingLib.RmLastPContent (rmLastPContent) where
import Content (PContent(..), PSection(..))
import ParsingLib.CheckLastContent (checkLastContent)
import ParsingLib.GetLastPSection (getLastPSection)
import ParsingLib.RetrieveTitle (retrieveTitle)

rmLastPContent :: [String] -> Bool -> [PContent] -> [PContent]
rmLastPContent [] _ contenu = init contenu
rmLastPContent ("section":xs) True contenu
    | checkLastContent (last contenu)
    = (init contenu) ++ [PSectionContent 
    (PSection {title = retrieveTitle (last contenu), section_content
    = (rmLastPContent xs True (getLastPSection (last contenu)))})]
rmLastPContent ("section":xs) False contenu = rmLastPContent (xs) True contenu 
rmLastPContent (_:xs) isBody contenu = rmLastPContent (xs) isBody contenu
