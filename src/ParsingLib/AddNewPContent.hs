{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AddNewPContent
-}

module ParsingLib.AddNewPContent (addNewPContent) where
import Content (PContent(..), PSection(..))
import ParsingLib.CheckLastContent (checkLastContent)
import ParsingLib.GetLastPSection (getLastPSection)
import ParsingLib.RetrieveTitle (retrieveTitle)

addNewPContent :: [String] -> Bool -> PContent -> [PContent] -> [PContent]
addNewPContent [] _ newC contenu = contenu ++ [newC]
addNewPContent ("section":xs) True newC contenu
    | checkLastContent (last contenu) 
    = (init contenu) ++ [PSectionContent 
    (PSection {title = retrieveTitle (last contenu), section_content 
    = (addNewPContent xs True newC (getLastPSection (last contenu)))})]
addNewPContent ("section":xs) False newC contenu 
    = addNewPContent (xs) True newC contenu
addNewPContent (_:xs) isBody newC contenu
    = addNewPContent (xs) isBody newC contenu