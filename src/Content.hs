{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Content
-}

module Content (PHeader(..), PBody(..),
    PContent(..),
    PText(..), PTextType(..),
    PLink(..), PImage(..),
    PParagraph(..), PParagraphType(..), PSection(..), PCodeBlock(..),
    PList(..), PItem(..), PItemType(..)) where

data PHeader = PHeader {
    header_title :: String,
    author :: Maybe String,
    date :: Maybe String
} deriving Show

data PBody = PBody [PContent]
    deriving Show

data PContent = PParagraphContent PParagraph
    | PSectionContent PSection
    | PCodeBlockContent PCodeBlock
    | PListContent PList
    deriving Show

data PText = PText [PTextType]
    deriving (Show)

data PTextType = PString String
    | PBoldText PBold
    | PItalicText PItalic
    | PCodeText PCode
    deriving (Show)

data PBold = PBold [PTextType]
    deriving (Show)

data PItalic = PItalic [PTextType]
    deriving (Show)

data PCode = PCode [PTextType]
    deriving (Show)

data PLink = PLink {
    link_url :: String,
    content :: PText
} deriving (Show)

data PImage = PImage {
    image_url :: String,
    alt :: PText
} deriving (Show)

data PParagraph = PParagraph [PParagraphType]
    deriving (Show)

data PParagraphType = PTextParagraph PText
    | PLinkParagraph PLink
    | PImageParagraph PImage
    deriving (Show)

data PSection = PSection {
    title :: String,
    section_content :: [PContent]
} deriving (Show)

data PCodeBlock = PCodeBlock [String]
    deriving (Show)

data PList = PList [PItem]
    deriving (Show)

data PItem = PItem [PItemType]
    deriving (Show)

data PItemType = PParagraphItem PParagraph
    | PListItem PList
    deriving (Show)
