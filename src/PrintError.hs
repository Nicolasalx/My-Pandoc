{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- PrintError
-}

module PrintError (printErrorAndExit) where
import System.Exit
import System.IO (hPutStrLn, stderr)

printErrorAndExit :: String -> IO a
printErrorAndExit msg = hPutStrLn stderr msg >>
    exitWith (ExitFailure 84)
