{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DataStructMarkdown
-}

module ParseMarkdown.DataStructMarkdown (initializeDataParsing, DataParsing(..), TypeToAdd(..)) where

data TypeToAdd = None | Paragraph | Link | Image | CodeBlock | Section | Item
  deriving (Show, Eq)

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

    typeToAdd :: TypeToAdd,

    actualCodeBloc :: [String],

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

    typeToAdd = None,

    actualCodeBloc = [],

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
