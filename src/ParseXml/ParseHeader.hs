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

checkTitle :: PHeader -> Either String ()
checkTitle header =
    if null (header_title header)
        then Left "Erreur : Aucun titre trouvé dans l'en-tête"
        else Right ()

fillPHeader :: [String] -> Either String PHeader
fillPHeader [] = Right PHeader { header_title = "", author = Nothing, date = Nothing }
fillPHeader (x:xs)
    | Just ("<title>", value) <- parseString "<title>" x,
      Just (title, _) <- parseUntil '<' value,
      Right header <- fillPHeader xs =
          Right header { header_title = title }
    | Just ("<author>", value) <- parseString "<author>" x,
      Just (author, _) <- parseUntil '<' value,
      Right header <- fillPHeader xs =
          Right header { author = Just author }
    | Just ("<date>", value) <- parseString "<date>" x,
      Just (date, _) <- parseUntil '<' value,
      Right header <- fillPHeader xs =
          Right header { date = Just date }
    | elem '<' x = Right PHeader { header_title = "", author = Nothing, date = Nothing }
    | otherwise = Left "Erreur : Format d'en-tête invalide ou champ invalide"
