{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AddIndent
-}

module ExportFormat.AddIndent (addIndent) where

addIndent :: Int -> String
addIndent indent = replicate (indent * 4) ' '
