{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DataStructMarkdown
-}

data DataParsing = DataParsing
  {
    rule :: Maybe Int,
    isInLink :: Bool,
    isInImage :: Bool,
    isInParagraph :: Bool,
    levelSection :: Int,
    levelItem :: Int,
    remainingLines :: [String]
  } deriving (Show, Eq)
