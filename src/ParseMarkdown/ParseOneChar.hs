--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseOneChar
--

module ParseMarkdown.ParseOneChar(parseOneChar) where
import ParseMarkdown.DataStructMarkdown (DataParsing(..))

addCharToActualList :: Char -> DataParsing -> IO DataParsing
addCharToActualList c dataParsing = do
    return (dataParsing { actualList = actualList dataParsing ++ [c], nbReturnLines = 0})

parseOneChar :: Char -> DataParsing -> IO DataParsing
parseOneChar '`' dataParsing = do
    (addCharToActualList '`' dataParsing)

parseOneChar '-' dataParsing = do
    (addCharToActualList '-' dataParsing)

parseOneChar '[' dataParsing = do
    (addCharToActualList '[' dataParsing)

parseOneChar ']' dataParsing = do
    (addCharToActualList ']' dataParsing)

parseOneChar '!' dataParsing = do
    (addCharToActualList '!' dataParsing)

parseOneChar '(' dataParsing = do
    (addCharToActualList '(' dataParsing)

parseOneChar ')' dataParsing = do
    (addCharToActualList ')' dataParsing)

parseOneChar '#' dataParsing = do
    (addCharToActualList '#' dataParsing)

parseOneChar '*' dataParsing = do
    (addCharToActualList '*' dataParsing)

parseOneChar c dataParsing = do
    addCharToActualList c dataParsing
