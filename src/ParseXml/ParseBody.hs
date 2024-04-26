{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
-}

module ParseXml.ParseBody (parseBody) where

import Content (PContent(..), PParagraph(..), PParagraphType(..), PText(..),
    PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..),
    PCodeBlock(..), PList(..), PItem(..), PItemType(..), PLink(..), PImage(..))
import ParseJson.ParseFunction (appendPContent, initPContent, lastPContent)

-- parsing title

addTitle :: String -> PContent -> PContent
addTitle str (PSectionContent (PSection {title = _, 
    section_content = contenu})) = 
    PSectionContent (PSection {title = str, section_content = contenu})
addTitle _ _ = PSectionContent (PSection {title = "", section_content = []})

parseTitle :: [String] -> String -> String ->
    [PContent] -> Either String [PContent]
parseTitle _ _ [] _ = Left "Error: Missing symbol in title"
parseTitle state str suite contenu =
    parseBaseLoop (state ++ ["section"]) (getStrAfterKey ">" suite)
    (appendPContent state
        (addTitle (getStrUntil '"' str "") (lastPContent state contenu)) 
        ((initPContent state contenu)))

-- parsing paragraph

addParagraph :: String -> String -> PContent -> PContent
addParagraph "text" str (PParagraphContent (PParagraph list)) =
    PParagraphContent $ PParagraph $ list ++ [PTextParagraph 
    (PText [PString str])]
addParagraph "bold" str (PParagraphContent (PParagraph list)) =
    PParagraphContent $ PParagraph $ list ++ [PTextParagraph 
    (PText [PBoldText (PBold [PString str])])]
addParagraph "italic" str (PParagraphContent (PParagraph list)) =
    PParagraphContent $ PParagraph $ list ++ [PTextParagraph
    (PText [PItalicText (PItalic [PString str])])]
addParagraph "code" str (PParagraphContent (PParagraph list)) =
    PParagraphContent $ PParagraph $ list ++ [PTextParagraph
    (PText [PCodeText (PCode [PString str])])]
addParagraph "link" _ (PParagraphContent (PParagraph list)) =
    PParagraphContent $ PParagraph $ list ++ [PLinkParagraph
    (PLink {link_url = "", content = PText []})]
addParagraph "image" _ (PParagraphContent (PParagraph list)) =
    PParagraphContent $ PParagraph $ list ++ [PImageParagraph
    (PImage {image_url = "", alt = PText []})]
addParagraph _ _ _ = PParagraphContent $ PParagraph []

parseParagraph :: String -> [String] -> String ->
    String -> [PContent] -> Either String [PContent]
parseParagraph _ _ _ [] contenu = Right contenu 
parseParagraph typeStr state paragraph suite contenu =
    parseBaseLoop state suite (appendPContent state 
        (addParagraph typeStr paragraph (lastPContent state contenu))
        ((initPContent state contenu)))

-- Parsing codeblock

addCodeBlock :: String -> PContent -> PContent
addCodeBlock str (PCodeBlockContent (PCodeBlock list)) =
    PCodeBlockContent $ PCodeBlock $ list ++ [str]
addCodeBlock _ _ = PCodeBlockContent $ PCodeBlock []

parseCodeBlock :: [String] -> String -> String ->
    [PContent] -> Either String [PContent]
parseCodeBlock _ _ [] _ = Left "Error: Missing symbol in codeblock"
parseCodeBlock state str suite contenu =
    parseBaseLoop state suite (appendPContent state
        (addCodeBlock str (lastPContent state contenu))
        ((initPContent state contenu)))

-- parsing link

getLinkUrl :: String -> PParagraphType -> PParagraphType
getLinkUrl str (PLinkParagraph (PLink {link_url = "", content = contenu}))
    = PLinkParagraph (PLink {link_url = str, content = contenu})
getLinkUrl str (PLinkParagraph 
    (PLink {link_url = theTitle, content = PText []}))
    = PLinkParagraph 
    (PLink {link_url = theTitle, content = PText [PString str]})
getLinkUrl _ _ = PLinkParagraph (PLink {link_url = "", content = PText []})

addLinkUrl :: String -> PContent -> PContent
addLinkUrl str (PParagraphContent (PParagraph list)) 
    = PParagraphContent $ PParagraph $ init list ++ 
    [getLinkUrl str (last list)]
addLinkUrl _ _ = PParagraphContent $ PParagraph []

parseLinkUrl :: [String] -> String -> String ->
    [PContent] -> Either String [PContent]
parseLinkUrl _ _ [] _ = Left "Error: Missing symbol in link"
parseLinkUrl state link suite contenu = parseBaseLoop state suite
    (appendPContent state (addLinkUrl link (lastPContent state contenu))
        ((initPContent state contenu)))

-- parsing image

getImageUrl :: String -> PParagraphType -> PParagraphType
getImageUrl str (PImageParagraph (PImage {image_url = "", alt = contenu}))
    = PImageParagraph (PImage {image_url = str, alt = contenu})
getImageUrl str (PImageParagraph 
    (PImage {image_url = theTitle, alt = PText []})) 
    = PImageParagraph 
    (PImage {image_url = theTitle, alt = PText [PString str]})
getImageUrl _ _ = PImageParagraph (PImage {image_url = "", alt = PText []})

addImageUrl :: String -> PContent -> PContent
addImageUrl str (PParagraphContent (PParagraph list)) 
    = PParagraphContent $ PParagraph $ init list ++ 
    [getImageUrl str (last list)]
addImageUrl _ _ = PParagraphContent $ PParagraph []

parseImageUrl :: [String] -> String -> String ->
    [PContent] -> Either String [PContent]
parseImageUrl _ _ [] _ = Left "Error: Missing symbol in image"
parseImageUrl state link suite contenu = parseBaseLoop state suite
    (appendPContent state (addImageUrl link (lastPContent state contenu))
        ((initPContent state contenu)))

-- parsing list

addList :: String -> PContent -> PContent
addList str (PListContent (PList list)) =
    PListContent $ PList $ list ++ [PItem [(PParagraphItem
        (PParagraph [PTextParagraph (PText [PString str])]))]]
addList _ _ = PListContent $ PList []

parseList :: [String] -> String -> String ->
    [PContent] -> Either String [PContent]
parseList _ _ [] _ = Left "Error: Missing symbol in image"
parseList state str suite contenu = parseBaseLoop state suite
    (appendPContent state (addList str (lastPContent state contenu))
        ((initPContent state contenu)))

-- parsing loop

parseBaseLoop :: [String] -> String -> [PContent] -> Either String [PContent]
parseBaseLoop _ [] contenu = Right contenu
parseBaseLoop [] _ contenu = Right contenu
parseBaseLoop state str contenu
    | last state == "section" && getKey "<section" str = parseTitle state
        (getStrAfterKey "<section title=\"" str) str (appendPContent state
        (PSectionContent
        (PSection {title = "", section_content = []})) contenu)
    | last state == "section" && getKey "</section>" str = parseBaseLoop
        (init state) (getStrAfterKey "</section>" str) contenu
    | otherwise = startCodeBlock state str contenu

startCodeBlock :: [String] -> String -> [PContent] -> Either String [PContent]
startCodeBlock state str contenu
    | last state == "section" && getKey "<codeblock>" str = parseBaseLoop
        (state ++ ["codeblock"]) (getStrAfterKey "<codeblock>" str)
        (appendPContent state (PCodeBlockContent (PCodeBlock [])) contenu)
    | last state == "codeblock" && getKey "</codeblock>" str = parseBaseLoop
        (init state) (getStrAfterKey "</codeblock>" str) contenu
    | last state == "codeblock" && getKey "<paragraph>" str = parseBaseLoop
        (state ++ ["codeparagraph"]) (getStrAfterKey "<paragraph>" str) contenu
    | otherwise = startCodeParagraph state str contenu

startCodeParagraph :: [String] -> String ->
    [PContent] -> Either String [PContent]
startCodeParagraph state str contenu
    | last state == "codeparagraph" && getKey "</paragraph>" str =
        parseBaseLoop (init state) (getStrAfterKey "</paragraph>" str) contenu
    | last state == "codeparagraph" = parseCodeBlock state
        (getStrUntil '<' str "") ("<" ++ (getStrAfterKey "<" str)) contenu
    | otherwise = codeParagraph state str contenu

codeParagraph :: [String] -> String -> [PContent] -> Either String [PContent]
codeParagraph state str contenu
    | last state == "codeparagraph" && getKey "</paragraph>" str =
        parseBaseLoop (init state) (getStrAfterKey "</paragraph>" str) contenu
    | last state == "codeparagraph" = parseCodeBlock state
        (getStrUntil '<' str "") ("<" ++ (getStrAfterKey "<" str)) contenu
    | otherwise = formatList state str contenu

formatList :: [String] -> String -> [PContent] -> Either String [PContent]
formatList state str contenu
    | last state == "section" && getKey "<list>" str = parseBaseLoop
        (state ++ ["list"]) (getStrAfterKey "<list>" str)
        (appendPContent state (PListContent (PList [])) contenu)
    | last state == "list" && getKey "</list>" str = parseBaseLoop
        (init state) (getStrAfterKey "</list>" str) contenu
    | last state == "list" && getKey "<paragraph>" str = parseBaseLoop
        (state ++ ["listparagraph"]) (getStrAfterKey "<paragraph>" str) contenu
    | otherwise = listParagraph state str contenu

listParagraph :: [String] -> String -> [PContent] -> Either String [PContent]
listParagraph state str contenu
    | last state == "listparagraph" && getKey "</paragraph>" str =
        parseBaseLoop (init state)
        (getStrAfterKey "</paragraph>" str) contenu
    | last state == "listparagraph" = parseList state (getStrUntil '<' str "")
        ("<" ++ (getStrAfterKey "<" str)) contenu
    | otherwise = searchStartParagraph state str contenu

searchStartParagraph :: [String] -> String ->
    [PContent] -> Either String [PContent]
searchStartParagraph state str contenu
    | last state == "section" && getKey "<paragraph>" str = parseBaseLoop
        (state ++ ["paragraph"]) (getStrAfterKey "<paragraph>" str)
        (appendPContent state (PParagraphContent (PParagraph [])) contenu)
    | last state == "paragraph" && getKey "<bold>" str = parseBaseLoop
        (state ++ ["bold"]) (getStrAfterKey "<bold>" str) contenu
    | otherwise = formatBold state str contenu

formatBold :: [String] -> String -> [PContent] -> Either String [PContent]
formatBold state str contenu
    | last state == "bold" && getKey "</bold>" str = parseBaseLoop
        (init state) (getStrAfterKey "</bold>" str) contenu
    | last state == "bold" = parseParagraph "bold" state
        (getStrUntil '<' str "") ("<" ++ (getStrAfterKey "<" str)) contenu
    | last state == "paragraph" && getKey "<italic>" str =
        parseBaseLoop (state ++ ["italic"])
        (getStrAfterKey "<italic>" str) contenu
    | otherwise = formatItalic state str contenu

formatItalic :: [String] -> String -> [PContent] -> Either String [PContent]
formatItalic state str contenu
    | last state == "italic" && getKey "</italic>" str =
            parseBaseLoop (init state) (getStrAfterKey "</italic>" str) contenu
    | last state == "italic" = parseParagraph "italic" state
        (getStrUntil '<' str "") ("<" ++ (getStrAfterKey "<" str)) contenu
    | last state == "paragraph" && getKey "<code>" str =
        parseBaseLoop (state ++ ["code"]) (getStrAfterKey "<code>" str) contenu
    | otherwise = formatCode state str contenu

formatCode :: [String] -> String -> [PContent] -> Either String [PContent]
formatCode state str contenu
    | last state == "code" && getKey "</code>" str =
        parseBaseLoop (init state) (getStrAfterKey "</code>" str) contenu
    | last state == "code" = parseParagraph "code" state
        (getStrUntil '<' str "") ("<" ++ (getStrAfterKey "<" str)) contenu
    | otherwise = searchStartLink state str contenu

searchStartLink :: [String] -> String -> [PContent] -> Either String [PContent]
searchStartLink state str contenu
    | last state == "paragraph" && getKey "<link" str = parseBaseLoop
        (state ++ ["link"]) (getStrAfterKey "<link" str) contenu
    | last state == "link" && getKey "</link>" str = parseBaseLoop
        (init state) (getStrAfterKey "</link>" str) contenu
    | last state == "link" && getKey "\"" str = parseLinkUrl state
        (getStrUntil '\"' (tail str) "")
        (">" ++ (getStrAfterKey ">" str)) contenu
    | otherwise = searchLink state str contenu

searchLink :: [String] -> String -> [PContent] -> Either String [PContent]
searchLink state str contenu
    | last state == "link" && getKey ">" str = parseLinkUrl state
        (getStrUntil '<' (tail str) "")
        ("<" ++ (getStrAfterKey "<" str)) contenu
    | last state == "link" = parseParagraph "link" state
        (getStrUntil '<' str "") (getStrAfterKey "url=" str) contenu
    | otherwise = searchStartImage state str contenu

searchStartImage :: [String] -> String ->
    [PContent] -> Either String [PContent]
searchStartImage state str contenu
    | last state == "paragraph" && getKey "<image" str = parseBaseLoop
        (state ++ ["image"]) (getStrAfterKey "<image" str) contenu
    | last state == "image" && getKey "</image>" str = parseBaseLoop
        (init state) (getStrAfterKey "</image>" str) contenu
    | last state == "image" && getKey "\"" str = parseImageUrl state
        (getStrUntil '\"' (tail str) "")
        (">" ++ (getStrAfterKey ">" str)) contenu
    | otherwise = searchImage state str contenu    

searchImage :: [String] -> String -> [PContent] -> Either String [PContent]
searchImage state str contenu
    | last state == "image" && getKey ">" str = parseImageUrl state
        (getStrUntil '<' (tail str) "")
        ("<" ++ (getStrAfterKey "<" str)) contenu
    | last state == "image" = parseParagraph "image" state
        (getStrUntil '<' str "") (getStrAfterKey "url=" str) contenu
    | otherwise = searchText state str contenu

searchText :: [String] -> String -> [PContent] -> Either String [PContent]
searchText state str contenu
    | last state == "paragraph" && getKey "</paragraph>" str =
        parseBaseLoop (init state) (getStrAfterKey "</paragraph>" str) contenu
    | last state == "paragraph" = parseParagraph "text" state
        (getStrUntil '<' str "") ("<" ++ (getStrAfterKey "<" str)) contenu
    | otherwise = searchEndDocument state str contenu

searchEndDocument :: [String] -> String ->
    [PContent] -> Either String [PContent]
searchEndDocument state str contenu
    | getKey "</body>" str =
        parseBaseLoop state (getStrAfterKey "</body>" str) contenu
    | getKey "</document>" str =
        parseBaseLoop state (getStrAfterKey "</document>" str) contenu
    | head str == ' ' || head str == '\n' =
        parseBaseLoop state (tail str) contenu
    | otherwise = Left "Error: Unknown symbol"

-- parsing base

getStrUntil :: Char -> String -> String -> String
getStrUntil _ [] _ = []
getStrUntil c (x:xs) str
    | x == c = str
    | otherwise = getStrUntil c xs (str ++ [x])

getStrAfterKey :: String -> String -> String
getStrAfterKey _ [] = []
getStrAfterKey [] str = str
getStrAfterKey (x:xs) (y:ys)
    | x == y = getStrAfterKey xs ys
    | otherwise = getStrAfterKey (x:xs) ys

getKey :: String -> String -> Bool
getKey [] _ = True
getKey _ [] = False
getKey (x:xs) (y:ys)
    | x == y = getKey xs ys
    | otherwise = False

parseUntilBody :: String -> [PContent] -> Either String [PContent]
parseUntilBody [] _ = Left "No <body> tag found"
parseUntilBody str contenu 
    | getKey "<body>" str =
        parseBaseLoop ["section"] (getStrAfterKey "<body>" str) contenu
    | otherwise = parseUntilBody (tail str) contenu

parseBody :: String -> Either String [PContent]
parseBody file_content = parseUntilBody file_content []
