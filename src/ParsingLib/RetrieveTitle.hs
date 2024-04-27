{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- RetrieveTitle
-}

module ParsingLib.RetrieveTitle (retrieveTitle) where
import Content (PContent(..), PSection(..))

retrieveTitle :: PContent -> String
retrieveTitle (PSectionContent 
    (PSection {title = theTitle, section_content = _})) = theTitle
retrieveTitle _ = ""