{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Lib
-}

module ParsingLib.Lib (parseString, strcmp, parseJsonKey, strToWordArray, nth, checkIsInString, searchSymbol, cleanLine) where
import Data.Char (isSpace)

type Parser a = String -> Maybe (a , String)

-- parseChar :: Char -> Parser Char
-- parseChar c (x:xs)
--     | x == c = Just (c, xs)
-- parseChar _ _ = Nothings

parseString :: String -> Parser String
parseString [] input = Just ([], input)
parseString (s:str) (x:xs)
    | s == x, Just (parsed, rest) <- parseString str xs = Just (s:parsed, rest)
    | otherwise = Nothing
parseString _ _ = Nothing

strToWordArray2 :: String -> Char -> Bool
strToWordArray2 [] _ = True
strToWordArray2 (x:xs) c
    | x == c = False
    | otherwise = strToWordArray2 xs c

strToWordArray :: String -> String -> String -> [String]
strToWordArray _ [] [] = []
strToWordArray _ tmp [] = tmp : []
strToWordArray str tmp (x:xs)
    | strToWordArray2 str x = strToWordArray str (tmp ++ [x]) xs
    | length tmp == 0 = strToWordArray str [] xs
    | otherwise = tmp : strToWordArray str [] xs

checkIsInString :: String -> String -> Bool
checkIsInString [] _ = False
checkIsInString _ [] = False
checkIsInString (x:xs) str
    | x `elem` str = True
    | otherwise = checkIsInString xs str


strcmp :: Eq a => [a] -> [a] -> Bool
strcmp [] _ = True
strcmp _ [] = False
strcmp (x:xs) (y:ys)
    | x == y = strcmp xs ys
    | otherwise = False

parseJsonKey :: [String] -> Int -> Parser String
parseJsonKey [] _ _ = Nothing
parseJsonKey _ _ [] = Nothing
parseJsonKey (x:xs) n input
    | strcmp x input && n == 2 = parseJsonKey xs (n-1) input
    | n == 1 && ':' `elem` x = parseJsonKey xs (n-1) input
    | n == 0 && length x > 0 = Just (input, x)
    | otherwise = Nothing

nth :: Int -> [String] -> [String]
nth _ [] = []
nth 0 x = x
nth n (_:xs) = nth (n-1) xs

searchSymbol :: String -> String -> Bool
searchSymbol [] _ = True
searchSymbol _ [] = False
searchSymbol (x:xs) (y:ys)
    | x == y = searchSymbol xs ys
    | otherwise = searchSymbol (x:xs) ys

cleanLine :: String -> Maybe String
cleanLine [] = Nothing
cleanLine str = Just $ dropWhile isSpace str

-- parseUntilStr :: String -> String -> (Maybe String, String)
-- parseUntilStr _ [] = (Nothing, [])
-- parseUntilStr end str = extractUntil str
--   where
--     extractUntil [] = (Nothing, [])
--     extractUntil (x:xs)
--       | Just _ <- stripPrefix end (x:xs) = (Just [], xs)
--       | (Just result, rest) <- extractUntil xs = (Just (x:result), rest)
--       | otherwise = (Nothing, str)

-- addParagraph :: String -> PContent -> PContent
-- addParagraph str (PParagraphContent (PParagraph list)) = PParagraphContent (PParagraph (list ++ [PTextParagraph (PText [PString str])]))

-- lib bootstrap

-- parseAnyChar :: String -> Parser Char
-- parseAnyChar [] _ = Nothing
-- parseAnyChar _ [] = Nothing
-- parseAnyChar (c:cs) (x:xs)
--     | x == c = Just (c, xs)
--     | otherwise = parseAnyChar cs (x:xs)

-- parseOr :: Parser a -> Parser a -> Parser a
-- parseOr p1 p2 s = (p1 s) `or` (p2 s)
--     where
--         or Nothing Nothing = Nothing
--         or (Just a) _ = Just a
--         or _ (Just b) = Just b

-- parseAnd :: Parser a -> Parser b -> Parser (a, b)
-- parseAnd p1 p2 s = p1 s >>= \ (a, s') -> p2 s' >>= \ (b, s'') -> Just((a, b), s'')

-- parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
-- parseAndWith f p1 p2 s = p1 s >>= \ (a, s') -> p2 s' >>= \ (b, s'') -> Just(f a b, s'')

-- parseMany :: Parser a -> Parser [a]
-- parseMany f s = f s >>= \ (a, s') -> parseMany f s' >>= \ (as, s'') -> Just(a:as, s'')
