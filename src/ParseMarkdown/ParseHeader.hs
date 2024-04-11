--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseHeader
--

module ParseMarkdown.ParseHeader (parseHeader) where
import Lib (parseString)
import Content (PHeader(..))
import ParseMarkdown.DataStructMarkdown (DataParsing(..))

parseHeader :: [String] -> DataParsing -> IO (Either String PHeader, DataParsing)
parseHeader line dataParsing =
    parseEachLine line False [] dataParsing >>=
        \(listStringHeader, dataStructModified) ->
    checkErrorLine listStringHeader >>=
        \headerResult ->
    return $ either
        (\errorMsg -> (Left errorMsg, dataStructModified))
        (\header -> (checkTitle header, dataStructModified))
        headerResult

checkTitle :: PHeader -> Either String PHeader
checkTitle (PHeader { header_title = "" }) = Left "Error: No title has been entered or the header is not correct"
checkTitle header = Right header

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

parseEachLine :: [String] -> Bool -> [String] -> DataParsing -> IO (Either String [String], DataParsing)
parseEachLine [] _ acc dataParsing = return (Right acc, dataParsing)
parseEachLine (x:xs) isInBlock acc dataParsing
    | isInBlock && x /= "---" = parseEachLine xs isInBlock (acc ++ [x]) dataParsing
    | x == "---" && not isInBlock = parseEachLine xs True acc dataParsing
    | x == "---" && isInBlock = return (Right acc, dataParsing {remainingLines = xs})
    | null acc && x /= "---" = return (Left "Header need to begin in the first line and not have space or anything else before", dataParsing)
    | otherwise = parseEachLine xs isInBlock acc dataParsing
