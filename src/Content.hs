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
} deriving (Show, Eq)

data PBody = PBody [PContent]
    deriving (Show, Eq)

data PContent = PParagraphContent PParagraph
    | PSectionContent PSection
    | PCodeBlockContent PCodeBlock
    | PListContent PList
    deriving (Show, Eq)

data PText = PText [Either PTextType String]
    deriving (Show, Eq)

data PTextType = InItalic | InBold | InCode | OutItalic | OutBold | OutCode
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
