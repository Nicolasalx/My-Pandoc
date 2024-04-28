{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- GetLastPSection
-}

module ParsingLib.GetLastPSection (getLastPSection) where
import Content (PContent(..), PSection(..))

getLastPSection :: PContent -> [PContent]
getLastPSection (PSectionContent 
    (PSection {title = _, section_content = contenu})) = contenu
getLastPSection _ = []
