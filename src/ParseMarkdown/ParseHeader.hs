--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseHeader
--

module ParseMarkdown.ParseHeader (parseHeader) where
import Content (PHeader(..))

parseHeader :: [String] -> Either String PHeader
parseHeader [] = Left "error: empty input"
parseHeader lines = do
    let result = parseEachLine lines False []

parseEachLine :: [String] -> Bool -> [String] -> Either String [String]
parseEachLine [] False _ = Left "No header in the file"
parseEachLine _ True _ = Left "The header is not finished"
parseEachLine (line:rest) isInHeader listHeader
    | line == "---" && not isInHeader = parseEachLine rest True listHeader
    | line == "---" && isInHeader = Right listHeader
    | otherwise = parseEachLine rest isInHeader (listHeader ++ [line])