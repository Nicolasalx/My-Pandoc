{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- DataStructMarkdown
-}

module ParseMarkdown.DataStructMarkdown (initializeDataParsing, DataParsing(..), TypeToAdd(..), initializeDataText, TypeText(..), DataText(..), ElemTextType(..)) where
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

    levelText :: Int,

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
    basicStr = "",

    levelText = 0,

    isInBold = False,
    isInItalic = False,
    isInCode = False,

    contentText = PText [],

    indexListText = 0,

    precedentChar = ' ',
    listText = []
  }

data DataParsing = DataParsing
  {
    insertItem :: Bool,
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

    isInParagraph :: Bool,
    isInCodeblock :: Bool,
    levelSection :: Int,
    
    levelItem :: Int,
    preElemIsItem :: Bool,
    listItem :: [PItem],

    actualList :: String,
    nbReturnLines :: Int,
    remainingLines :: [String]
  } deriving (Show, Eq)

initializeDataParsing :: DataParsing
initializeDataParsing = DataParsing
  {
    insertItem = False,
    insertLinkOrImage = False,
    isInContentLink = False,
    isInAltImage = False,
    isInUrlLink = False,
    isInUrlImage = False,

    contentLink = "",
    altImg = "",

    paragraph = [],

    urlLink = "",
    urlImg = "",

    typeToAdd = None,

    hasFillCodeBlock = False,
    actualCodeBlock = [],

    isInParagraph = False,
    isInCodeblock = False,
    levelSection = 0,
    
    levelItem = 0,
    preElemIsItem = False,
    listItem = [],

    actualList = "",
    nbReturnLines = 0,
    remainingLines = []
  }
