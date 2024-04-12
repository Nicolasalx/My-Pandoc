--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AppendElemToDataStruct
--

module ParsingLib.AppendElemToDataStruct (addNewElemToContent) where
import Content (PContent(..), PText(..), )

addNewElemToContent :: PContent -> [PContent] -> [PContent]
addNewElemToContent actualContent allContent = allContent ++ [actualContent]
