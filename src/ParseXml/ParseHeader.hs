{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseHeader
-}

module ParseXml.ParseHeader (parseHeader) where
import Content (PHeader(..))
import ParsingLib.Lib (strcmp, parseJsonKey, strToWordArray, nth)
import Data.List (isInfixOf, isPrefixOf)
import Debug.Trace (trace)

<<<<<<< HEAD
checkTitle :: PHeader -> Either String PHeader
checkTitle pHeader
    | header_title pHeader == "" = Left "Error: No title found in header"
    | otherwise = Right pHeader

parseEachHeaderLine :: [String] -> PHeader -> Either String PHeader
parseEachHeaderLine [] _ = Left "Error: No closing bracket found"
parseEachHeaderLine (x:xs) pHeader
    | "document" `isInfixOf` x = parseEachHeaderLine xs pHeader
    | "header title" `isInfixOf` x = parseEachHeaderLine xs pHeader { header_title = head xs }
    | "author" `isPrefixOf` x = parseEachHeaderLine xs pHeader { author = Just (head xs) }
    | "date" `isPrefixOf` x = parseEachHeaderLine xs pHeader { date = Just (head xs) }
    | "/header" `isInfixOf` x = Right pHeader
    | otherwise = parseEachHeaderLine xs pHeader

searchForHeader :: [String] -> Either String PHeader
searchForHeader [] = Left "Error: No header found after bracket"
searchForHeader (x:xs)
    | "document" `isInfixOf` x = parseEachHeaderLine xs 
        PHeader {header_title = "", author = Nothing, date = Nothing}
    | otherwise = searchForHeader xs

parseHeader :: String -> Either String PHeader
parseHeader [] = Left "Empty file"
parseHeader (x:xs) = searchForHeader (strToWordArray "<>=\"" "" (x:xs))
=======
fillPHeader :: [String] -> Either String PHeader
fillPHeader [] = Right PHeader
  { header_title = "", author = Nothing, date = Nothing }
fillPHeader (x:xs)
    | Just cleanedLine <- cleanLine x,
      Just ("<header title=\"", value) <-
        parseString "<header title=\"" cleanedLine,
      Just (title, _) <- parseUntil "\">" value,
      Right header <- fillPHeader xs =
          Right header { header_title = title }
    | Just cleanedLine <- cleanLine x,
      Just ("<author>", value) <- parseString "<author>" cleanedLine,
      Just (authorResult, _) <- parseUntil "</author>" value,
      Right header <- fillPHeader xs =
          Right header { author = Just authorResult }
    | Just cleanedLine <- cleanLine x,
      Just ("<date>", value) <- parseString "<date>" cleanedLine,
      Just (dateResult, _) <- parseUntil "</date>" value,
      Right header <- fillPHeader xs =
          Right header { date = Just dateResult }
    | elem '<' x = fillPHeader xs
    | otherwise = Left "Erreur : Format d'en-tête invalide ou champ invalide"
>>>>>>> 621a0a5cb8ceecb22cdfb2295d78bc9ef84dc59e
