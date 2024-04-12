{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DataStructMarkdown
-}

module ParseMarkdown.DataStructMarkdown (initializeDataParsing, DataParsing(..)) where

data DataParsing = DataParsing
  {
    isInContentLink :: Bool,
    isInAltImage :: Bool,

    isInUrlLink :: Bool,
    isInUrlImage :: Bool,

    contentLink :: String,
    altImg :: String,

    urlLink :: String,
    urlImg :: String,

    isInParagraph :: Bool,
    isInCodeblock :: Bool,
    levelSection :: Int,
    levelItem :: Int,
    nbStars :: Int,
    nbBackTick :: Int,
    actualList :: String,
    nbReturnLines :: Int,
    lastCharacter :: Char,
    remainingLines :: [String]
  } deriving (Show, Eq)

initializeDataParsing :: DataParsing
initializeDataParsing = DataParsing
  {
    isInContentLink = False,
    isInAltImage = False,

    isInUrlLink = False,
    isInUrlImage = False,

    contentLink = "",
    altImg = "",

    urlLink = "",
    urlImg = "",

    isInParagraph = False,
    isInCodeblock = False,
    levelSection = 0,
    levelItem = 0,
    nbStars = 0,
    nbBackTick = 0,
    actualList = "",
    nbReturnLines = 0,
    lastCharacter = ' ',
    remainingLines = []
  }
