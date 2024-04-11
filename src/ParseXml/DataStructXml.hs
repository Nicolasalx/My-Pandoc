{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DataStructXml
-}

module ParseXml.DataStructXml (initializeDataParsing, DataParsing(..)) where

data DataParsing = DataParsing
  {
    inHeader :: Bool,
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
    inHeader = False,
    isInLink = False,
    isInImage = False,
    isInParagraph = False,
    levelSection = 0,
    levelItem = 0,
    remainingLines = []
  }
