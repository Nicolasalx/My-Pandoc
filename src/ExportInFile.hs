{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportInFile
-}

module ExportInFile (exportInFile) where
import System.IO

putInFile :: FilePath -> String -> IO ()
putInFile filepath str = withFile filepath WriteMode (\fd -> hPutStr fd str)

exportInFile :: Maybe FilePath -> String -> IO ()
exportInFile (Just filepath) str = putInFile filepath str
exportInFile (Nothing) str = putStr str
