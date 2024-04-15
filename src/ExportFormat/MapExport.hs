{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- MapExport
-}

module ExportFormat.MapExport (mapExport) where

mapExport :: (a -> [b]) -> [b] -> [a] -> [b]
mapExport _ _ [] = []
mapExport func _ [first] = func first
mapExport func sep (first:remain) = func first
    ++ sep
    ++ mapExport func sep remain
