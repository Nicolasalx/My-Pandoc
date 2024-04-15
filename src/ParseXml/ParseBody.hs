{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
-}

module ParseXml.ParseBody (parseBody) where
import Content (PBody(..))
import ParseXml.DataStructXml (DataParsing(..), initializeDataParsing)
import ParsingLib.Lib (strcmp)

parseBody :: String -> IO (Either String PBody)
parseBody file_content = return $ Right (PBody [])

checkTypeBalise :: String -> DataParsing -> DataParsing
checkTypeBalise str dataParsing
    | strcmp str "<paragraph>" = dataParsing { isInParagraph = True }
    | strcmp str "<section>" = dataParsing { levelSection = levelSection dataParsing + 1 }
    | strcmp str "<codeblock>" = dataParsing { isInCodeBlock = True }
    | strcmp str "<bold>" = dataParsing { isInBold = True }
    | strcmp str "<italic>" = dataParsing { isInItalic = True }
    | strcmp str "<image url=" = dataParsing { isInImage = True }
    | strcmp str "<link url=" = dataParsing { isInLink = True }
    | otherwise = dataParsing

checkEndBalise :: String -> DataParsing -> DataParsing
checkEndBalise str dataParsing
    | strcmp str "</paragraph>" = dataParsing { isInParagraph = False }
    | strcmp str "</codeblock>" = dataParsing { isInCodeBlock = False }
    | strcmp str "</bold>" = dataParsing { isInBold = False }
    | strcmp str "</italic>" = dataParsing { isInItalic = False }
    | strcmp str "</image>" = dataParsing { isInImage = False }
    | strcmp str "</link>" = dataParsing { isInLink = False }
    | otherwise = dataParsing

