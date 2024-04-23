{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseHeader
-}

module ParseMarkdown.ParseHeader (parseHeader) where
import ParsingLib.Lib (parseString)
import Content (PHeader(..))
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import ParseMarkdown.ParseElem.SkipSpaces (skipSpaces)

parseHeader :: [String] -> DataParsing -> (Either String PHeader, DataParsing)
parseHeader line dataParsing
    | Left errorMsg <- headerResult = (Left errorMsg, dataStructModified)
    | Right header <- headerResult = (checkTitle header, dataStructModified)
    where
        (listStringHeader, dataStructModified) =
            parseEachLine line False [] dataParsing
        headerResult = checkErrorLine listStringHeader

checkTitle :: PHeader -> Either String PHeader
checkTitle (PHeader { header_title = "" }) =
  Left "Error: The header is not correct"
checkTitle header = Right header

checkErrorLine :: Either String [String] -> (Either String PHeader)
checkErrorLine (Left errorMsg) = (Left errorMsg)
checkErrorLine (Right list2) = computeHeader list2

computeHeader :: [String] -> (Either String PHeader)
computeHeader list = eitherResult (fillPHeader list)
    where
        eitherResult (Left err) = Left err
        eitherResult (Right header) = Right header

fillPHeader :: [String] -> Either String PHeader
fillPHeader [] = Right PHeader
  { header_title = "", author = Nothing, date = Nothing }
fillPHeader (x:xs)
    | Just ("title:", value) <- parseString "title:" x,
      Right header <- fillPHeader xs =
        Right header { header_title = (skipSpaces 100 value) }
    | Just ("author:", value) <- parseString "author:" x,
      Right header <- fillPHeader xs =
        Right header { author = Just (skipSpaces 100 value) }
    | Just ("date:", value) <- parseString "date:" x,
      Right header <- fillPHeader xs =
        Right header { date = Just (skipSpaces 100 value) }
    | otherwise = Left "Error: Invalid header format or field"

parseEachLine :: [String] -> Bool -> [String] ->
  DataParsing -> (Either String [String], DataParsing)
parseEachLine [] _ acc dataParsing = (Right acc, dataParsing)
parseEachLine (x:xs) isInBlock acc dataParsing
    | isInBlock && x /= "---" =
        parseEachLine xs isInBlock (acc ++ [x]) dataParsing
    | "---" <- x, not isInBlock = parseEachLine xs True acc dataParsing
    | isInBlock, "---" <- x = (Right acc, dataParsing { remainingLines = xs })
    | null acc, x /= "---" =
      (Left "Header needs to begin in the first line", dataParsing)
    | otherwise = parseEachLine xs isInBlock acc dataParsing

