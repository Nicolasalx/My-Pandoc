{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- AddLineBreak
-}

module ExportFormat.AddLineBreak (addLineBreak) where

addLineBreak :: String -> String
addLineBreak body
    | null body = ""
    | last body == '\n' = body
    | otherwise = body ++ "\n"
