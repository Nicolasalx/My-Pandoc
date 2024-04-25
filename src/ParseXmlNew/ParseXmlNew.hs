{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseXmlNew
-}

module ParseXmlNew.ParseXmlNew (parseXmlNew) where
import Content (PHeader(..), PBody(..))
import ParseXmlNew.DataStructXml (DataParsing(..), initializeDataParsing)
import ParseXmlNew.ParseBody (parseBody)
import ParseXmlNew.ParseHeader (parseHeader)

launchParsingHeader :: Either String (PHeader, String) -> Either String (PHeader, Either String PBody)
launchParsingHeader (Left header_error) = Left header_error
launchParsingHeader (Right header) = Right ((fst header), (parseBody (snd header) (initializeDataParsing { remainingLines = (snd header) })))

launchParsing :: Either String (PHeader, Either String PBody) -> Either String (PHeader, PBody)
launchParsing (Left header_error) = Left header_error
launchParsing (Right (_, Left body_error)) = Left body_error
launchParsing (Right (header, Right body)) = Right (header, body)

parseXmlNew :: String -> IO (Either String (PHeader, PBody))
parseXmlNew content = return (launchParsing (launchParsingHeader (parseHeader content)))
