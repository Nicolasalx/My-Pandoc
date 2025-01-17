{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseHeader
-}

module ParseJson.ParseHeader (parseHeader) where
import Content (PHeader(..))
import ParsingLib.StrToWordArray (strToWordArray)
import ParsingLib.Strcmp (strcmp)
import ParsingLib.Nth (nth)
import ParsingLib.ParseJsonKey (parseJsonKey)

searchForHeader :: [String] -> Int -> Either String PHeader
searchForHeader [] _ = Left "Error: No header found after bracket"
searchForHeader (x:xs) 0
    | '{' `elem` x = searchForHeader xs 1
searchForHeader (x:xs) 1
    | strcmp "header" x = parseEachHeaderLine xs 
    PHeader {header_title = "", author = Nothing, date = Nothing}
searchForHeader _ _ = Left "Error: No header found after bracket"

checkTitle :: PHeader -> Either String PHeader
checkTitle pHeader
    | header_title pHeader == "" = Left "Error: No title found in header"
    | otherwise = Right pHeader

checkVirgule :: [String] -> PHeader -> Either String PHeader
checkVirgule [] _ = Left "Error: No closing bracket found"
checkVirgule (x:xs) pHeader
    | '}' `elem` x = checkTitle pHeader
    | ',' `elem` x = parseEachHeaderLine xs pHeader
    | otherwise = Left "Error: Invalid json format"

parseEachHeaderLine :: [String] -> PHeader -> Either String PHeader
parseEachHeaderLine [] _ = Left "Error: No closing bracket found"
parseEachHeaderLine (x:xs) pHeader
    | '{' `elem` x = parseEachHeaderLine xs pHeader
    | Just ("title", value) <- parseJsonKey (x:xs) 2 "title" =
        checkVirgule (nth 2 xs) pHeader { header_title = value }
    | Just ("author", value) <- parseJsonKey (x:xs) 2 "author" =
        checkVirgule (nth 2 xs) pHeader { author = Just value }
    | Just ("date", value) <- parseJsonKey (x:xs) 2 "date" =
        checkVirgule (nth 2 xs) pHeader { date = Just value }
    | otherwise = Left x

parseHeader :: String -> Either String PHeader
parseHeader [] = Left "Empty file"
parseHeader (x:xs) = searchForHeader (strToWordArray "\"" "" (x:xs)) 0
