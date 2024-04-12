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
    nbStars :: Int,
    nbBackTick :: Int,
    actualList :: String,
    nbReturnLines :: Int,
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
    nbStars = 0,
    nbBackTick = 0,
    actualList = "",
    nbReturnLines = 0,
    remainingLines = []
  }
