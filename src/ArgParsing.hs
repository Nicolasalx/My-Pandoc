{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ArgParsing
-}

module ArgParsing (launchArgParsing, Format(..), PandocArg(..)) where
import PrintError (printErrorAndExit)
import System.Environment(getArgs)

data Format = NotProvided | JSON | XML | MarkDown
    deriving Show

data PandocArg = PandocArg {
    in_file :: Either String FilePath,
    out_format :: Format,
    out_file :: Either String FilePath,
    in_format :: Format
} deriving (Show)

defaultPandocArg :: PandocArg
defaultPandocArg =
    PandocArg {
        in_file = Left "Missing input file.",
        out_format = NotProvided,
        out_file = Left "Not provided.",
        in_format = NotProvided
    }

parsePandocArg :: PandocArg -> [String] -> Either String PandocArg
parsePandocArg conf ("-i" : second : remain) =
    parsePandocArg (conf {in_file = Right second}) remain
parsePandocArg conf ("-f" : "json" : remain) =
    parsePandocArg (conf {out_format = JSON }) remain
parsePandocArg conf ("-f" : "xml" : remain) =
    parsePandocArg (conf {out_format = XML}) remain
parsePandocArg conf ("-f" : "markdown" : remain) =
    parsePandocArg (conf {out_format = MarkDown}) remain
parsePandocArg conf ("-o" : second : remain) =
    parsePandocArg (conf {out_file = Right second}) remain
parsePandocArg conf ("-e" : "json" : remain) =
    parsePandocArg (conf {in_format = JSON}) remain
parsePandocArg conf ("-e" : "xml" : remain) =
    parsePandocArg (conf {in_format = XML}) remain
parsePandocArg conf ("-e" : "markdown" : remain) =
    parsePandocArg (conf {in_format = MarkDown}) remain
parsePandocArg pandocArg [] = Right pandocArg
parsePandocArg _ _ = Left "Invalid arg."

checkArgValidity :: Either String PandocArg -> IO (PandocArg)
checkArgValidity (Right (PandocArg (Left msg) _ _ _)) = printErrorAndExit msg
checkArgValidity (Right (PandocArg _ NotProvided _ _)) = printErrorAndExit "Output format not provided."
checkArgValidity (Right pandocArg) = return (pandocArg)
checkArgValidity (Left msg) = printErrorAndExit msg

launchArgParsing :: IO (PandocArg)
launchArgParsing = getArgs >>=
    \argv -> (checkArgValidity (parsePandocArg defaultPandocArg argv))
