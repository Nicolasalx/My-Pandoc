{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseHeader
-}

module ParseXml.ParseHeader (fillPHeader) where
import ParsingLib.Lib (parseString)
import Content (PHeader(..), PBody(..))
import Data.Maybe (fromMaybe)
import ParseXml.DataStructXml (initializeDataParsing)
import ParsingLib.Lib (strToWordArray, nth, strcmp, parseUntil, cleanLine)

fillPHeader :: [String] -> Either String PHeader
fillPHeader [] = Right PHeader { header_title = "", author = Nothing, date = Nothing }
fillPHeader (x:xs)
    | Just cleanedLine <- cleanLine x,
      Just ("<header title=\"", value) <- parseString "<header title=\"" cleanedLine,
      Just (title, _) <- parseUntil "\">" value,
      Right header <- fillPHeader xs =
          Right header { header_title = title }
    | Just cleanedLine <- cleanLine x,
      Just ("<author>", value) <- parseString "<author>" cleanedLine,
      Just (author, _) <- parseUntil "</author>" value,
      Right header <- fillPHeader xs =
          Right header { author = Just author }
    | Just cleanedLine <- cleanLine x,
      Just ("<date>", value) <- parseString "<date>" cleanedLine,
      Just (date, _) <- parseUntil "</date>" value,
      Right header <- fillPHeader xs =
          Right header { date = Just date }
    | elem '<' x = fillPHeader xs
    | otherwise = Left "Erreur : Format d'en-tête invalide ou champ invalide"
