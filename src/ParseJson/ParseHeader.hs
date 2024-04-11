--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseHeader
--

module ParseJson.ParseHeader (parseJsonHeader) where
import Content (PHeader(..))
import Lib (parseJsonKey, strToWordArray)

checkBracket :: String -> Int -> Bool
checkBracket [] 0 = True
checkBracket [] _ = False 
checkBracket ('{':xs) n = checkBracket xs (n+1)
checkBracket ('}':xs) n = checkBracket xs (n-1)
checkBracket ('[':xs) n = checkBracket xs (n+1)
checkBracket (']':xs) n = checkBracket xs (n-1)
checkBracket (_:xs) n = checkBracket xs n

searchForHeader :: [String] -> Either String PHeader
searchForHeader [] = Left "Error: No header found after bracket"
searchForHeader (x:xs)
    | parseJsonKey "\"header\":" 4 x /= Nothing = parseEachHeaderLine xs PHeader { header_title = "", author = Nothing, date = Nothing }
    | otherwise = searchForHeader xs

parseEachHeaderLine :: [String] -> PHeader -> Either String PHeader
parseEachHeaderLine [] x = Right x
parseEachHeaderLine (x:xs) pHeader
    | Just ("", value) <- parseJsonKey "\"title\":" 4 x =
        parseEachHeaderLine xs pHeader { header_title = value }
    | Just ("", value) <- parseJsonKey "\"author\":" 4 x =
        parseEachHeaderLine xs pHeader { author = Just value }
    | Just ("", value) <- parseJsonKey "\"date\":" 4 x =
        parseEachHeaderLine xs pHeader { date = Just value }
    | otherwise = parseEachHeaderLine xs pHeader

parseJsonHeader :: String -> IO (Either String PHeader)
parseJsonHeader [] = return $ Left "Empty file"
parseJsonHeader (x:xs) 
    | checkBracket (x:xs) 0 = return $ searchForHeader (strToWordArray "{}[]," "" (x:xs))
    | otherwise = return $ Left "Error: Invalid json format"
