{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- CheckLastContent
-}

module ParsingLib.CheckLastContent (checkLastContent) where
import Content (PContent(..), PSection(..))

checkLastContent :: PContent -> Bool
checkLastContent (PSectionContent
    (PSection {title = _, section_content = _})) = True
checkLastContent _ = False
