{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
-}

module ParseJson.ParseBody (parseBody) where
import Content (PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PLink(..), PImage(..))
import ParsingLib.AppendPContent (appendPContent)
import ParsingLib.NotBracketChar (notBracketChar)
import ParsingLib.InitPContent (initPContent)
import ParsingLib.LastPContent (lastPContent)
import ParsingLib.StrToWordArray (strToWordArray)
import ParsingLib.Nth (nth)
import ParsingLib.CheckIsInString (checkIsInString)
import ParsingLib.Strcmp (strcmp)

-- Adding title to section

addTitle :: String -> PContent -> PContent
addTitle str (PSectionContent (PSection {title = _, 
    section_content = contenu})) = 
    PSectionContent (PSection {title = str, section_content = contenu})
addTitle _ _ = PSectionContent (PSection {title = "", section_content = []})

parseTitle :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseTitle _ [] _ = Left "Error: Missing symbol in title"
parseTitle state (x:xs) contenu
    | checkIsInString (['A'..'Z'] ++ ['a'..'z']) x = 
    parseSymbol (state ++ ["section"]) xs (appendPContent state 
    (addTitle x (lastPContent state contenu)) 
    ((initPContent state contenu)))
    | otherwise = parseSymbol (state ++ ["section"]) 
    (x:xs) (appendPContent state (addTitle "" (lastPContent state contenu))
    ((initPContent state contenu)))

-- Parsing paragraph

addParagraph :: String -> String -> PContent -> PContent
addParagraph "text" str (PParagraphContent (PParagraph list)) 
    = PParagraphContent $ PParagraph $ list ++ [PTextParagraph 
    (PText [PString str])]
addParagraph "bold" str (PParagraphContent (PParagraph list)) 
    = PParagraphContent $ PParagraph $ list ++ [PTextParagraph 
    (PText [PBoldText (PBold [PString str])])]
addParagraph "italic" str (PParagraphContent (PParagraph list)) 
    = PParagraphContent $ PParagraph $ list ++ [PTextParagraph 
    (PText [PItalicText (PItalic [PString str])])]
addParagraph "code" str (PParagraphContent (PParagraph list)) 
    = PParagraphContent $ PParagraph $ list ++ [PTextParagraph 
    (PText [PCodeText (PCode [PString str])])]
addParagraph "link" _ (PParagraphContent (PParagraph list)) 
    = PParagraphContent $ PParagraph $ list ++ [PLinkParagraph 
    (PLink {link_url = "", content = PText []})]
addParagraph "image" _ (PParagraphContent (PParagraph list)) 
    = PParagraphContent $ PParagraph $ list ++ [PImageParagraph 
    (PImage {image_url = "", alt = PText []})]
addParagraph _ _ _ = PParagraphContent $ PParagraph []

parseParagraph :: String -> [String] -> [String] -> [PContent] -> Either String [PContent]
parseParagraph _ _ [] _ = Left "Error: Missing symbol in paragraph"
parseParagraph typeStr state (x:xs) contenu 
    = parseSymbol state xs (appendPContent state 
    (addParagraph typeStr x (lastPContent state contenu))
    ((initPContent state contenu)))

-- Parsing codeblock

addCodeBlock :: String -> PContent -> PContent
addCodeBlock str (PCodeBlockContent (PCodeBlock list)) 
    = PCodeBlockContent $ PCodeBlock $ list ++ [str]
addCodeBlock _ _ = PCodeBlockContent $ PCodeBlock []

parseCodeBlock :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseCodeBlock _ [] _ = Left "Error: Missing symbol in codeblock"
parseCodeBlock state (x:xs) contenu = parseSymbol state xs 
    (appendPContent state (addCodeBlock x (lastPContent state contenu))
    ((initPContent state contenu)))

-- Parsing list

addList :: String -> PContent -> PContent
addList str (PListContent (PList list)) = 
    PListContent $ PList $ list ++ [PItem [(PParagraphItem 
    (PParagraph [PTextParagraph (PText [PString str])]))]]
addList _ _ = PListContent $ PList []

parseList :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseList _ [] _ = Left "Error: Missing symbol in list"
parseList state (x:xs) contenu = parseSymbol state xs 
    (appendPContent state (addList x (lastPContent state contenu)) 
    ((initPContent state contenu)))

-- Parsing link and Image

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

parseLinkUrl :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseLinkUrl _ [] _ = Left "Error: Missing symbol in link"
parseLinkUrl state (x:xs) contenu = parseSymbol state xs 
    (appendPContent state (addLinkUrl x (lastPContent state contenu)) 
    ((initPContent state contenu)))

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

parseImageUrl :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseImageUrl _ [] _ = Left "Error: Missing symbol in image"
parseImageUrl state (x:xs) contenu 
    = parseSymbol state xs (appendPContent state 
    (addImageUrl x (lastPContent state contenu)) ((initPContent state contenu)))

-- check quel est le type du text et call la bonne fonction

parseTextList :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseTextList [] _ _ = Left "Error: Missing symbol in text"
parseTextList _ [] _ = Left "Error: Missing } in text"
parseTextList s ("alt":xs) contenu
    | last s == "inimage" = parseSymbol s xs contenu
parseTextList s (x:xs) contenu
    | last s == "contentlink" = parseLinkUrl s (x:xs) contenu
    | last s == "altimage" = parseImageUrl s (x:xs) contenu
    | (last s == "bold" || last s == "italic" || last s == "code")
    = parseParagraph (last s) s (x:xs) contenu
    | last s == "incodeblock" = parseCodeBlock s (x:xs) contenu
    | last s == "codeblock" = parseCodeBlock s (x:xs) contenu
    | otherwise = parseTextList2 s (x:xs) contenu

parseTextList2 :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseTextList2 [] _ _ = Left "Error: Missing symbol in text"
parseTextList2 _ [] _ = Left "Error: Missing } in text"
parseTextList2 s (x:xs) contenu
    | last s == "inlist" = parseList s (x:xs) contenu
    | last s == "list" = parseList s (x:xs) contenu
    | last s == "content" = parseParagraph "text" s (x:xs)
    (appendPContent s (PParagraphContent (PParagraph [])) contenu)
    | otherwise = Left "No key found"

parseTextLink :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseTextLink [] _ _ = Left "Error: Missing symbol in text"
parseTextLink _ [] _ = Left "Error: Missing } in text"
parseTextLink state ("image":xs) contenu
    | last state == "?" = parseParagraph "image" 
    ((init state) ++ ["beforeImage"]) ("image":xs) contenu
parseTextLink state ("bold":xs) contenu
    | last state == "?" = parseSymbol ((init state) ++ ["bold"])  xs contenu
parseTextLink state ("italic":xs) contenu
    | last state == "?" = parseSymbol ((init state) ++ ["italic"])  xs contenu
parseTextLink state ("code":xs) contenu
    | last state == "?" = parseSymbol ((init state) ++ ["code"])  xs contenu
parseTextLink state ("url":xs) contenu
    | last state == "inlink" = parseLinkUrl state (nth 1 xs) contenu
parseTextLink state ("content":xs) contenu
    | last state == "inlink" = parseSymbol state xs contenu
parseTextLink state ("url":xs) contenu
    | last state == "inimage" = parseImageUrl state (nth 1 xs) contenu
parseTextLink state (x:xs) contenu = parseTextList state (x:xs) contenu

parseTextCodeBlock :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseTextCodeBlock [] _ _ = Left "Error: Missing symbol in text"
parseTextCodeBlock _ [] _ = Left "Error: Missing } in text"
parseTextCodeBlock state ("codeblock":xs) contenu
    | last state == "?" = parseSymbol ((init state) ++ ["beforeCodeblock"]) xs
    (appendPContent state (PCodeBlockContent (PCodeBlock [])) contenu)
parseTextCodeBlock state ("list":xs) contenu
    | last state == "?" = parseSymbol ((init state) ++ ["beforeList"]) xs
    (appendPContent state (PListContent (PList [])) contenu)
parseTextCodeBlock state ("link":xs) contenu
    | last state == "?" = parseParagraph "link"
    ((init state) ++ ["beforeLink"]) ("link":xs) contenu
parseTextCodeBlock state (x:xs) contenu = parseTextLink state (x:xs) contenu

parseText :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseText [] _ _ = Left "Error: Missing symbol in text"
parseText _ [] _ = Left "Error: Missing } in text"
parseText state ("section":xs) contenu
    | last state == "?" = parseSymbol ((init state) ++ ["beforeSection"]) xs
    (appendPContent state (PSectionContent 
    (PSection {title = "", section_content = []})) contenu)
parseText state ("title":xs) contenu
    | last state == "beforeSection" = parseTitle state (nth 1 xs) contenu
parseText state ("content":xs) contenu
    | last state == "section" = parseSymbol state xs contenu
parseText state (x:xs) contenu
    | last state == "paragraph" = parseParagraph "text" state (x:xs) contenu
    | otherwise = parseTextCodeBlock state (x:xs) contenu

-- rempli la list d'état avec le type de contenu puis appelle parseText

parseSymbolParagraph :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolParagraph _ [] _ = Left "Error: Missing symbol in text"
parseSymbolParagraph state (x:xs) contenu
    | head x == '{' && last state == "paragraph" 
    = parseSymbol (state ++ ["?"]) (tail x:xs) contenu
    | head x == '{' && last state == "beforeLink" 
    = parseSymbol (state ++ ["inlink"]) (tail x:xs) contenu
    | head x == '{' && last state == "beforeImage" 
    = parseSymbol (state ++ ["inimage"]) (tail x:xs) contenu
    | otherwise = parseSymbolSection state (x:xs) contenu

parseSymbolSection :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolSection _ [] _ 
    = Left "Error: Missing symbol when creating section in text"
parseSymbolSection state (x:xs) contenu
    | head x == '{' && last state == "beforeSection"
    = parseSymbol state (tail x:xs) contenu
    | otherwise = parseSymbolCodeblock state (x:xs) contenu

parseSymbolCodeblock2 :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolCodeblock2 _ [] _ 
    = Left "Error: Missing symbol in codeblock, list or link in text"
parseSymbolCodeblock2 state (x:xs) contenu
    | head x == '[' && last state == "list" 
    = parseSymbol (state ++ ["inlist"]) (tail x:xs) contenu
    | head x == '[' && last state == "inlink" 
    = parseSymbol (state ++ ["contentlink"]) (tail x:xs) contenu
    | head x == '[' && last state == "inimage" 
    = parseSymbol (state ++ ["altimage"]) (tail x:xs) contenu
    | otherwise = Left "Error: Missing symbol"

parseSymbolCodeblock :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolCodeblock _ [] _ = 
    Left "Error: Missing symbol in codeblock, list or link in text"
parseSymbolCodeblock state (x:xs) contenu
    | head x == '[' && last state == "beforeCodeblock"
    = parseSymbol (state ++ ["codeblock"]) (tail x:xs) contenu
    | head x == '[' && last state == "codeblock"
    = parseSymbol (state ++ ["incodeblock"]) (tail x:xs) contenu
    | head x == '[' && last state == "beforeList"
    = parseSymbol (state ++ ["list"]) (tail x:xs) contenu
    | otherwise = parseSymbolCodeblock2 state (x:xs) contenu

parseSymbol2 :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbol2 _ [] _ = Left "Error: Missing } in text"
parseSymbol2 state (x:xs) contenu
    | head x == '{' && last state == "content" 
    = parseSymbol (state ++ ["?"]) (tail x:xs) contenu
    | notBracketChar x = parseSymbol state (tail x:xs) contenu
    | otherwise = parseSymbolParagraph state (x:xs) contenu

parseSymbol :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbol _ [] _ = Left "Error: Missing } in text"
parseSymbol _ ([]:[]) contenu = Right contenu
parseSymbol state ([]:xs) contenu = parseText state xs contenu
parseSymbol state (x:xs) contenu
    | head x == '}' || head x == ']' 
    = parseSymbol (init state) (tail x:xs) contenu
    | head x == '[' && last state == "section" 
    = parseSymbol (state ++ ["content"]) (tail x:xs) contenu
    | head x == '[' && last state == "content" 
    = parseSymbol (state ++ ["paragraph"]) (tail x:xs) 
    (appendPContent state (PParagraphContent (PParagraph [])) contenu)
    | otherwise = parseSymbol2 state (x:xs) contenu

-- Parsing de la base

initDataParsing :: [String] -> [String]
initDataParsing x = x ++ ["section"]

parseHeader :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseHeader _ [] _ = Left "Error: Missing } in header"
parseHeader dataParsing (x:xs) contenu
    | '}' `elem` x = parseBaseLoop dataParsing xs contenu
    | otherwise = parseHeader dataParsing xs contenu

parseBaseLoop :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseBaseLoop _ [] contenu = Right contenu
parseBaseLoop dataParsing (x:xs) contenu 
    | strcmp "header" x = parseHeader dataParsing xs contenu
    | strcmp "body" x = parseSymbol (initDataParsing dataParsing) xs contenu
    | otherwise = Right contenu

enterInSection :: [String] -> [String] -> [PContent] -> Either String [PContent]
enterInSection _ [] contenu = Right contenu
enterInSection dataParsing (x:xs) contenu
    | '{' `elem` x = parseBaseLoop dataParsing xs contenu
    | otherwise = Left "Error: Missing { in section"

parseBody :: String -> Either String [PContent]
parseBody file_content =
    enterInSection [] (strToWordArray "\"" "" file_content) []
