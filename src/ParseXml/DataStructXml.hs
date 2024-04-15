{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DataStructXml
-}

module ParseXml.DataStructXml (initializeDataParsing, DataParsing(..)) where

data DataParsing = DataParsing
  {
    isInHeader :: Bool,
    isInLink :: Bool,
    isInImage :: Bool,
    isInParagraph :: Bool,
    isInCodeBlock :: Bool,
    isInBold :: Bool,
    isInItalic :: Bool,
    levelSection :: Int,
    levelItem :: Int,
    remainingLines :: [String]
  } deriving (Show, Eq)

initializeDataParsing :: DataParsing
initializeDataParsing = DataParsing
  {
    isInHeader = False,
    isInLink = False,
    isInImage = False,
    isInParagraph = False,
    isInCodeBlock = False,
    isInBold = False,
    isInItalic = False,
    levelSection = 0,
    levelItem = 0,
    remainingLines = []
  }
