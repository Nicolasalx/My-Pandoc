{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseString
-}

module ParsingLib.ParseString (parseString, runParser) where

data Parser a = Parser (String -> Maybe (a , String))

instance Functor Parser where
    fmap func (Parser parser) =
        Parser (fmap (\(result, remain) -> (func result, remain)) . parser)

runParser :: Parser String -> String -> Maybe (String, String)
runParser (Parser parser) = parser

parseChar :: Char -> Maybe (String, String) -> Maybe (String, String)
parseChar c (Just (parsed, remain)) = Just (c : parsed, remain)
parseChar _ Nothing = Nothing

parseStringHelper :: String -> String -> Maybe (String, String)
parseStringHelper _ [] = Nothing
parseStringHelper [] _ = Nothing
parseStringHelper (first_in : remain_in) (first_str : remain_str)
    | first_in == first_str = parseChar first_str
        (runParser (parseString remain_str) remain_in)
    | otherwise = Nothing

parseString :: String -> Parser String
parseString [] = Parser (\input -> Just ([], input))
parseString str = Parser (\input -> parseStringHelper input str)
