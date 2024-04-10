{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportFormatData
-}

module ExportFormat.ExportFormatData (ExportFormat(..)) where

data ExportFormat = JSON | XML | MD
    deriving (Show)
