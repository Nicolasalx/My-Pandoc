{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ExportInFile
-}

module ExportInFile (exportInFile) where
import Control.Exception
import PrintError (printErrorAndExit)
import System.IO

putInFile :: FilePath -> String -> IO ()
putInFile filepath str =
    catch (withFile filepath WriteMode (\fd -> hPutStr fd str)) handleException
    where
        handleException :: IOException -> IO ()
        handleException _ = printErrorAndExit "Invalid output file."

exportInFile :: Maybe FilePath -> String -> IO ()
exportInFile (Just filepath) str = putInFile filepath str
exportInFile (Nothing) str = putStr str
