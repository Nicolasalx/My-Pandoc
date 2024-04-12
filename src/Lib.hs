module Lib (parseString, parseJsonKey, strToWordArray) where

type Parser a = String -> Maybe (a , String)

-- parseChar :: Char -> Parser Char
-- parseChar c (x:xs)
--     | x == c = Just (c, xs)
-- parseChar _ _ = Nothing

parseString :: String -> Parser String
parseString [] input = Just ([], input)
parseString (s:str) (x:xs)
  | s == x, Just (parsed, rest) <- parseString str xs = Just (s:parsed, rest)
  | otherwise = Nothing
parseString _ _ = Nothing

parseJsonKey :: String -> Int -> Parser String
parseJsonKey [] _ [] = Just ([], [])
parseJsonKey [_] _ [] = Nothing
parseJsonKey (_:_:_) _ [] = Nothing
parseJsonKey [] state (' ':xs) = parseJsonKey [] state (xs)
parseJsonKey [] state ('\n':xs) = parseJsonKey [] state (xs)
parseJsonKey [] state ('\"':xs) = parseJsonKey [] state (xs)
parseJsonKey [] state input
  | last input == '\n' || last input == ' ' = parseJsonKey [] state (init input)
  | last input == '\"' = Just ([], init input)
  | otherwise = Nothing
parseJsonKey (s:str) state (x:xs)
  | x /= '\"' && state == 4 = parseJsonKey (s:str) 4 (xs) 
  | x == '\"' && s == '\"' && state == 4 = parseJsonKey str 3 (xs)
  | s == x && state == 3 = parseJsonKey str 3 (xs)
  | x == '\"' && s == '\"' && state == 3 = parseJsonKey str 2 (xs)
  | s == ':' && state == 2 = parseJsonKey str 1 (xs)
  | otherwise = Nothing

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
