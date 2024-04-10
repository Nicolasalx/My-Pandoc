--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseHeader
--

module ParseMarkdown.ParseHeader (parseHeader) where
import Lib (parseString)
import Content (PHeader(..))

parseHeader :: [String] -> IO (Either String PHeader)
parseHeader line = checkTitle (checkErrorLine (parseEachLine line False []))

checkTitle :: IO (Either String PHeader) -> IO (Either String PHeader)
checkTitle action = action >>= (\result -> return $ result >>= checkHeaderTitle)

checkHeaderTitle :: PHeader -> Either String PHeader
checkHeaderTitle header
    | length (header_title header) == 0 = Left "Error: No title has been entered or the header is not correct"
    | otherwise = Right header

checkErrorLine :: Either String [String] -> IO (Either String PHeader)
checkErrorLine (Left errorMsg) = return (Left errorMsg)
checkErrorLine (Right list2) = computeHeader list2

computeHeader :: [String] -> IO (Either String PHeader)
computeHeader list = return $ eitherResult (fillPHeader list)
    where
        eitherResult (Left err) = Left err
        eitherResult (Right header) = Right header

fillPHeader :: [String] -> Either String PHeader
fillPHeader [] = Right PHeader { header_title = "", author = Nothing, date = Nothing }
fillPHeader (x:xs)
    | Just ("title:", value) <- parseString "title:" x,
      Right header <- fillPHeader xs =
          Right header { header_title = value }
    | Just ("author:", value) <- parseString "author:" x,
      Right header <- fillPHeader xs =
          Right header { author = Just value }
    | Just ("date:", value) <- parseString "date:" x,
      Right header <- fillPHeader xs =
          Right header { date = Just value }
    | otherwise = Left "Error: Invalid header format or field"

parseEachLine :: [String] -> Bool -> [String] -> Either String [String]
parseEachLine [] _ acc = Right acc
parseEachLine (x:xs) isInBlock acc
    | isInBlock && x /= "---" = parseEachLine xs isInBlock (acc ++ [x])
    | x == "---" && not isInBlock = parseEachLine xs True acc
    | x == "---" && isInBlock = parseEachLine xs False acc
    | otherwise = parseEachLine xs isInBlock acc
