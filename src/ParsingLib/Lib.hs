<<<<<<< HEAD
module ParsingLib.Lib (parseString, strcmp, parseJsonKey, strToWordArray, nth, searchSymbol, parseUntil, cleanLine, addParagraph) where
=======
module ParsingLib.Lib (parseString, strcmp, parseJsonKey, strToWordArray, nth, parseUntil, cleanLine, Parser, searchSymbol, checkIsInString) where
>>>>>>> ad587e1f8a9c5b89effeabf806af897304a31f9f
import Data.Char (isSpace)
import Content (PHeader(..), PBody(..),
    PContent(..),
    PText(..), PTextType(..), PBold(..), PItalic(..), PCode(..),
    PLink(..), PImage(..),
    PParagraph(..), PParagraphType(..), PSection(..), PCodeBlock(..),
    PList(..), PItem(..), PItemType(..))

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

strcmp :: String -> String -> Bool
strcmp [] [] = True
strcmp [] _ = False
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
<<<<<<< HEAD
    
=======

>>>>>>> ad587e1f8a9c5b89effeabf806af897304a31f9f
cleanLine :: String -> Maybe String
cleanLine [] = Nothing
cleanLine str = Just $ dropWhile isSpace str

parseUntil :: String -> String -> Maybe (String, String)
parseUntil _ "" = Nothing
parseUntil target str
    | strcmp target (take (length target) str) = Just ("", drop (length target) str)
    | otherwise = do
        (parsed, rest) <- parseUntil target (tail str)
        return (head str:parsed, rest)

addParagraph :: String -> PContent -> PContent
addParagraph str (PParagraphContent (PParagraph list)) = PParagraphContent (PParagraph (list ++ [PTextParagraph (PText [PString str])]))

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
