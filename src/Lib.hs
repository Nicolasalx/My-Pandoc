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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
