--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
--

module ParseJson.ParseBody (parseBody) where
import Content (PContent(..), PParagraph(..), PParagraphType(..), PText(..), PTextType(..))
import ParseJson.DataStructJson (DataParsing(..), initializeDataParsing)
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)
import ParsingLib.Lib (strToWordArray, strcmp, searchSymbol)

--p Parsing paragraph 

createParagraph :: String -> PContent
createParagraph x = PParagraphContent (PParagraph [PTextParagraph (PText [PString x])])

parseParagraph :: DataParsing -> [String] -> [PContent] -> Either String [PContent]
parseParagraph _ [] _ = Left "Error: Missing ] in paragraph"
parseParagraph dataParsing (x:xs) content
    | searchSymbol "]," x = parseContentLoop dataParsing xs content
    | otherwise = parseContentLoop dataParsing xs (addNewElemToContent (createParagraph x) content)

-- Parsing du body

parseContentLoop :: DataParsing -> [String] -> [PContent] -> Either String [PContent]
parseContentLoop _ [] _ = Left "Error: Missing } in content"
parseContentLoop dataParsing (x:xs) content 
    | searchSymbol "[[" x =  parseParagraph dataParsing xs content
    | searchSymbol "}" x && not (',' `elem` x) = Right content
    | otherwise = parseContentLoop dataParsing xs content

-- Parsing de la base

parseHeader :: DataParsing -> [String] -> [PContent] -> Either String [PContent]
parseHeader _ [] _ = Left "Error: Missing } in header"
parseHeader dataParsing (x:xs) content
    | '}' `elem` x = parseBaseLoop dataParsing xs content
    | otherwise = parseHeader dataParsing xs content

parseBaseLoop :: DataParsing -> [String] -> [PContent] -> Either String [PContent]
parseBaseLoop _ [] content = Right content
parseBaseLoop dataParsing (x:xs) content 
    | strcmp "header" x = parseHeader dataParsing xs content
    | strcmp "body" x = parseContentLoop dataParsing xs content
    | otherwise = Right content

enterInSection :: DataParsing -> [String] -> [PContent] -> Either String [PContent]
enterInSection _ [] content = Right content
enterInSection dataParsing (x:xs) content
    | '{' `elem` x = parseBaseLoop dataParsing xs content
    | otherwise = Left "Error: Missing { in section"

parseBody :: String -> IO (Either String [PContent])
parseBody file_content = return $ enterInSection initializeDataParsing (strToWordArray "\"" "" file_content) []
