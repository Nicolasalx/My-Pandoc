{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- Content
-}

module Content (PContent(..)) where

data PContent = PTextContent PText
    | PLinkContent PLink
    | PImageContent PImage
    | PParagraphContent PParagraph
    | PSectionContent PSection
    | PCodeBlockContent PCodeBlock
    | PListContent PList
    deriving Show

data PText = PText [Either PTextType String]
    deriving (Show)

data PTextType = InItalic | InBold | InCode | OutItalic | OutBold | OutCode
    deriving (Show)

data PLink = PLink {
    link_url :: String,
    content :: PText
} deriving (Show)

data PImage = PImage {
    image_url :: String,
    alt :: PText
} deriving (Show)

data PParagraph = PParagraph PText
    deriving (Show)

data PSection = PSection {
    title :: PText,
    section_content :: [PContent]
} deriving (Show)

data PCodeBlock = PCodeBlock String
    deriving (Show)

data PList = PList [PItem]
    deriving (Show)

data PItem = PItem [PContent]
    deriving (Show)
