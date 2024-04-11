{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DataStructMarkdown
-}

module ParseMarkdown.DataStructMarkdown (initializeDataParsing, DataParsing(..)) where

data DataParsing = DataParsing
  {
    isInLink :: Bool,
    isInImage :: Bool,
    isInParagraph :: Bool,
    levelSection :: Int,
    levelItem :: Int,
    remainingLines :: [String]
  } deriving (Show, Eq)

initializeDataParsing :: DataParsing
initializeDataParsing = DataParsing
  {
    isInLink = False,
    isInImage = False,
    isInParagraph = False,
    levelSection = 0,
    levelItem = 0,
    remainingLines = []
  }
