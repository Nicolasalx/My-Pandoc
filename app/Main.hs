{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Main
-}

module Main (main) where
import ArgParsing (launchArgParsing, Format(..), PandocArg(..))
import PrintError (printErrorAndExit)
import Control.Exception (catch, IOException)

import ParseJson.ParseJson (parseJson)
import ParseXml.ParseXml (parseXml)
import ParseMarkdown.ParseMarkdown (parseMarkdown)
import Content (PHeader(..), PBody(..))

getFileContent :: PandocArg -> IO (String)
getFileContent (PandocArg (Right filepath) _ _ _) =
    catch (readFile filepath) handleException >>= \file_content ->
        return (file_content)
    where
        handleException :: IOException -> IO String
        handleException _ = printErrorAndExit "Invalid File path."
getFileContent _ = printErrorAndExit "Fail to get file content."

determineParser :: FilePath -> String -> IO (Either String (PHeader, PBody))
determineParser filepath content
    | drop (length filepath - 5) filepath == ".json" = return (parseJson content)
    | drop (length filepath - 4) filepath == ".xml" = return (parseXml content)
    | drop (length filepath - 3) filepath == ".md" = return (parseMarkdown content)
    | otherwise = printErrorAndExit "Unknow file type." -- try to execute all parser

getParsingRes :: Either String (PHeader, PBody) -> IO ((PHeader, PBody))
getParsingRes (Right resParsing) = return resParsing
getParsingRes (Left msg) = printErrorAndExit msg

launchParsing :: PandocArg -> String -> IO ((PHeader, PBody))
launchParsing (PandocArg _ _ _ JSON) content = getParsingRes (parseJson content)
launchParsing (PandocArg _ _ _ XML) content = getParsingRes (parseXml content)
launchParsing (PandocArg _ _ _ MarkDown) content = getParsingRes (parseMarkdown content)
launchParsing (PandocArg (Right filepath) _ _ NotProvided) content = getParsingRes =<< determineParser filepath content
launchParsing _ _ = printErrorAndExit "Error while launching parsing."

main :: IO ()
main = do
    arg <- launchArgParsing
    file_content <- getFileContent arg
    res_parsing <- (launchParsing arg file_content)
    print res_parsing
