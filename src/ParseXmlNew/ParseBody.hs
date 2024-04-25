{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
-}

module ParseXmlNew.ParseBody (parseBody) where
import Content (PBody(..), PContent(..))
import Data.List (isPrefixOf)
import ParseXmlNew.DataStructXml (DataParsing(..))

parseParagraph :: String -> DataParsing -> Either String PContent
parseParagraph content dataParsing = Left "Paragraph not implemented"

parseSection :: String -> DataParsing -> Either String PContent
parseSection content dataParsing = Left "Section not implemented"

parseCodeBlock :: String -> DataParsing -> Either String PContent
parseCodeBlock content dataParsing = Left "CodeBlock not implemented"

parseList :: String -> DataParsing -> Either String PContent
parseList content dataParsing = Left "List not implemented"

parseBodyType :: String -> DataParsing -> Either String PContent
parseBodyType (' ' : remain) dataParsing = parseBodyType remain dataParsing
parseBodyType ('\n' : remain) dataParsing = parseBodyType remain dataParsing
parseBodyType ('\t' : remain) dataParsing = parseBodyType remain dataParsing
-- parseBodyType [] dataParsing = Left "end"
parseBodyType remain dataParsing
    | isPrefixOf "<paragraph>" remain = parseParagraph remain dataParsing -- insert in section
    | isPrefixOf "<section" remain = parseSection remain dataParsing
    | isPrefixOf "<codeblock>" remain = parseCodeBlock remain dataParsing
    | isPrefixOf "<list>" remain = parseList remain dataParsing
    | otherwise = Left ("Invalid body content: \"" ++ remain ++ "\"")

parseBodyHelper :: Either String PContent -> Either String PBody
parseBodyHelper (Left error) = Left error
parseBodyHelper (Right content) = Right (PBody [content])

parseBody :: String -> DataParsing -> Either String PBody
parseBody remain dataParsing = parseBodyHelper (parseBodyType remain dataParsing)
