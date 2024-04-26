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
