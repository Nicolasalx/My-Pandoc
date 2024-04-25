{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DataStructXml
-}

module ParseXmlNew.DataStructXml (DataParsing(..), initializeDataParsing) where
import Content (PParagraphType(..), PText(..), PItem(..))

data TypeToAdd = None | Paragraph | Link | Image | CodeBlock | Section | Item
  deriving (Show, Eq)

data TypeText = Bold | Italic | Code
  deriving (Show, Eq)

data ElemTextType = TString String
    | TBold TypeText
    | TItalic TypeText
    | TCode TypeText
    deriving (Show, Eq)

data DataText = DataText
  {
    basicStr :: String,
    isInBold :: Bool,
    isInItalic :: Bool,
    isInCode :: Bool,
    contentText :: PText,
    indexListText :: Int,
    precedentChar :: Char,
    listText :: [ElemTextType]
  } deriving (Show, Eq)

initializeDataText :: DataText
initializeDataText = DataText
  {
    basicStr = "", isInBold = False, isInItalic = False,
    isInCode = False, contentText = PText [], indexListText = 0,
    precedentChar = ' ', listText = []
  }

data DataParsing = DataParsing
  {
    hasAnalyzedImg :: Bool,
    insertLinkOrImage :: Bool,
    isInContentLink :: Bool,
    isInAltImage :: Bool,
    isInUrlLink :: Bool,
    isInUrlImage :: Bool,
    contentLink :: String,
    altImg :: String,
    paragraph :: [PParagraphType],
    urlLink :: String,
    urlImg :: String,
    typeToAdd :: TypeToAdd,
    hasFillCodeBlock :: Bool,
    actualCodeBlock :: [String],
    isInCodeblock :: Bool,
    levelSection :: Int,
    levelItem :: Int,
    listItem :: [PItem],
    actualList :: String,
    nbReturnLines :: Int,
    remainingLines :: String
  } deriving (Show, Eq)

initializeDataParsing :: DataParsing
initializeDataParsing = DataParsing
  {
    hasAnalyzedImg = False, insertLinkOrImage = False, isInContentLink = False,
    isInAltImage = False, isInUrlLink = False, isInUrlImage = False,
    contentLink = "", altImg = "", paragraph = [], urlLink = "", urlImg = "",
    typeToAdd = None, hasFillCodeBlock = False, actualCodeBlock = [],
    isInCodeblock = False, levelSection = 0, levelItem = 0, listItem = [],
    actualList = "", nbReturnLines = 0, remainingLines = ""
  }
