module Lib
    ( someFunc
    ) where


type Parser a = String -> Maybe (a , String)

parseChar :: Char -> Parser Char
parseChar c (x:xs)
  | x == c = Just (c, xs)
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Nothing
parseAnyChar _ [] = Nothing
parseAnyChar (c:cs) (x:xs)
  | x == c = Just (c, xs)
  | otherwise = parseAnyChar cs (x:xs)

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 s = (p1 s) `or` (p2 s)
    where
        or Nothing Nothing = Nothing
        or (Just a) _ = Just a
        or _ (Just b) = Just b

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 s = p1 s >>= \ (a, s') -> p2 s' >>= \ (b, s'') -> Just((a, b), s'')

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 s = p1 s >>= \ (a, s') -> p2 s' >>= \ (b, s'') -> Just(f a b, s'')

parseMany :: Parser a -> Parser [a]
parseMany f s = f s >>= \ (a, s') -> parseMany f s' >>= \ (as, s'') -> Just(a:as, s'')
