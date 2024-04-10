{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Main
-}

module Main (main) where
import ArgParsing (launchArgParsing)
import LaunchParsing (getFileContent, launchParsing)

main :: IO ()
main = do
    arg <- launchArgParsing
    file_content <- getFileContent arg
    res_parsing <- (launchParsing arg file_content)
    print res_parsing
