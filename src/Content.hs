{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Content
-}

module Content (PHeader(..), PBody(..),
    PContent(..),
    PText(..), PTextType(..), PBold(..), PItalic(..), PCode(..),
    PLink(..), PImage(..),
    PParagraph(..), PParagraphType(..), PSection(..), PCodeBlock(..),
    PList(..), PItem(..), PItemType(..)) where

data PHeader = PHeader {
    header_title :: String,
    author :: Maybe String,
    date :: Maybe String
} deriving (Show, Eq)

data PBody = PBody [PContent]
    deriving (Show, Eq)

data PContent = PParagraphContent PParagraph
    | PSectionContent PSection
    | PCodeBlockContent PCodeBlock
    | PListContent PList
    deriving (Show, Eq)

data PText = PText [PTextType]
    deriving (Show, Eq)

data PTextType = PString String
    | PBoldText PBold
    | PItalicText PItalic
    | PCodeText PCode
    deriving (Show, Eq)

data PBold = PBold [PTextType]
    deriving (Show, Eq)

data PItalic = PItalic [PTextType]
    deriving (Show, Eq)

data PCode = PCode [PTextType]
    deriving (Show, Eq)

data PLink = PLink {
    link_url :: String,
    content :: PText
} deriving (Show, Eq)

data PImage = PImage {
    image_url :: String,
    alt :: PText
} deriving (Show, Eq)

data PParagraph = PParagraph [PParagraphType]
    deriving (Show, Eq)

data PParagraphType = PTextParagraph PText
    | PLinkParagraph PLink
    | PImageParagraph PImage
    deriving (Show, Eq)

data PSection = PSection {
    title :: String,
    section_content :: [PContent]
} deriving (Show, Eq)

data PCodeBlock = PCodeBlock [String]
    deriving (Show, Eq)

data PList = PList [PItem]
    deriving (Show, Eq)

data PItem = PItem [PItemType]
    deriving (Show, Eq)

data PItemType = PParagraphItem PParagraph
    | PListItem PList
    deriving (Show, Eq)
