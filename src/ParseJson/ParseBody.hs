--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
--

module ParseJson.ParseBody (parseBody) where
import Content (PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PLink(..), PImage(..))
import ParsingLib.Lib (strToWordArray, strcmp, nth, checkIsInString)
import ParseJson.ParseFunction (notBracketChar, appendPContent, initPContent, lastPContent)
import Debug.Trace

-- Adding title to section

addTitle :: String -> PContent -> PContent
addTitle str (PSectionContent (PSection {title = _, section_content = contenu})) = PSectionContent (PSection {title = str, section_content = contenu})

parseTitle :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseTitle state (x:xs) contenu
    | checkIsInString (['A'..'Z'] ++ ['a'..'z']) x = parseSymbol (state ++ ["section"]) xs (appendPContent state (addTitle x (lastPContent state contenu)) ((initPContent state contenu)))
    | otherwise = parseSymbol (state ++ ["section"]) (x:xs) (appendPContent state (addTitle "" (lastPContent state contenu)) ((initPContent state contenu)))

-- Parsing paragraph

addParagraph :: String -> String -> PContent -> PContent
addParagraph "text" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PTextParagraph (PText [PString str])]
addParagraph "bold" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PTextParagraph (PText [PBoldText (PBold [PString str])])]
addParagraph "italic" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PTextParagraph (PText [PItalicText (PItalic [PString str])])]
addParagraph "code" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PTextParagraph (PText [PCodeText (PCode [PString str])])]
addParagraph "link" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PLinkParagraph (PLink {link_url = "", contenu = PText []})]
addParagraph "image" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PImageParagraph (PImage {image_url = "", alt = PText []})]

parseParagraph :: String -> [String] -> [String] -> [PContent] -> Either String [PContent]
parseParagraph typeStr state (x:xs) contenu = parseSymbol state xs (appendPContent state (addParagraph typeStr x (lastPContent state contenu)) ((initPContent state contenu)))

-- Parsing codeblock

addCodeBlock :: String -> PContent -> PContent
addCodeBlock str (PCodeBlockContent (PCodeBlock list)) = PCodeBlockContent $ PCodeBlock $ list ++ [str]

parseCodeBlock :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseCodeBlock state (x:xs) contenu = parseSymbol state xs (appendPContent state (addCodeBlock x (lastPContent state contenu)) ((initPContent state contenu)))

-- Parsing list

addList :: String -> PContent -> PContent
addList str (PListContent (PList list)) = PListContent $ PList $ list ++ [PItem [(PParagraphItem (PParagraph [PTextParagraph (PText [PString str])]))]]

parseList :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseList state (x:xs) contenu = parseSymbol state xs (appendPContent state (addList x (lastPContent state contenu)) ((initPContent state contenu)))

-- Parsing link and Image

getLinkUrl :: String -> PParagraphType -> PParagraphType
getLinkUrl str (PLinkParagraph (PLink {link_url = "", contenu = contenu})) = PLinkParagraph (PLink {link_url = str, contenu = contenu})
getLinkUrl str (PLinkParagraph (PLink {link_url = theTitle, contenu = PText []})) = PLinkParagraph (PLink {link_url = theTitle, contenu = PText [PString str]})

addLinkUrl :: String -> PContent -> PContent
addLinkUrl str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ init list ++ [getLinkUrl str (last list)]

parseLinkUrl :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseLinkUrl state (x:xs) contenu = parseSymbol state xs (appendPContent state (addLinkUrl x (lastPContent state contenu)) ((initPContent state contenu)))

getImageUrl :: String -> PParagraphType -> PParagraphType
getImageUrl str (PImageParagraph (PImage {image_url = "", alt = alt})) = PImageParagraph (PImage {image_url = str, alt = alt})
getImageUrl str (PImageParagraph (PImage {image_url = theTitle, alt = PText []})) = PImageParagraph (PImage {image_url = theTitle, alt = PText [PString str]})

addImageUrl :: String -> PContent -> PContent
addImageUrl str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ init list ++ [getImageUrl str (last list)]

parseImageUrl :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseImageUrl state (x:xs) contenu = parseSymbol state xs (appendPContent state (addImageUrl x (lastPContent state contenu)) ((initPContent state contenu)))


-- check quel est le type du text et call la bonne fonction

parseText :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseText [] _ _ = Left "Error: Missing symbol in text"
parseText _ [] contenu = Left "Error: Missing } in text"
parseText state (x:xs) contenu
    | s == "paragraph" = parseParagraph "text" state (x:xs) contenu
    | s == "?" && x == "section" = parseSymbol ((init state) ++ ["beforeSection"]) xs (appendPContent state (PSectionContent (PSection {title = "", section_content = []})) contenu)
    | s == "beforeSection" && x == "title" = parseTitle state (nth 1 xs) contenu
    | s == "section" && x == "contenu" = parseSymbol state xs contenu
    | s == "?" && x == "codeblock" = parseSymbol ((init state) ++ ["beforeCodeblock"]) xs (appendPContent state (PCodeBlockContent (PCodeBlock [])) contenu)
    | s == "?" && x == "list" = parseSymbol ((init state) ++ ["beforeList"]) xs (appendPContent state (PListContent (PList [])) contenu)
    | s == "?" && x == "link" = parseParagraph "link" ((init state) ++ ["beforeLink"]) (x:xs) contenu
    | s == "?" && x == "image" = parseParagraph "image" ((init state) ++ ["beforeImage"]) (x:xs) contenu
    | s == "?" && (x == "bold" || x == "italic" || x == "code") = parseSymbol ((init state) ++ [x])  xs contenu
    | s == "inlink" && x == "url" = parseLinkUrl state (nth 1 xs) contenu
    | s == "inlink" && x == "contenu" = parseSymbol state xs contenu
    | s == "inimage" && x == "url" = parseImageUrl state (nth 1 xs) contenu
    | s == "inimage" && x == "alt" = parseSymbol state xs contenu
    | s == "contentlink" = parseLinkUrl state (x:xs) contenu
    | s == "altimage" = parseImageUrl state (x:xs) contenu
    | (s == "bold" || s == "italic" || s == "code") = parseParagraph s state (x:xs) contenu
    | s == "incodeblock" = parseCodeBlock state (x:xs) contenu
    | s == "inlist" = parseList state (x:xs) contenu
    | otherwise = Left "No key found"
    where 
        s = last state
        
-- rempli la list d'état avec le type de contenu puis appelle parseText

parseSymbolParagraph :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolParagraph _ [] _ = Left "Error: Missing symbol in text"
parseSymbolParagraph state (x:xs) contenu
    | head x == '{' && last state == "paragraph" = parseSymbol (state ++ ["?"]) (tail x:xs) contenu
    | head x == '{' && last state == "beforeLink" = parseSymbol (state ++ ["inlink"]) (tail x:xs) contenu
    | head x == '{' && last state == "beforeImage" = parseSymbol (state ++ ["inimage"]) (tail x:xs) contenu
    | otherwise = parseSymbolSection state (x:xs) contenu

parseSymbolSection :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolSection _ [] _ = Left "Error: Missing symbol when creating section in text"
parseSymbolSection state (x:xs) contenu
    | head x == '{' && last state == "beforeSection" = parseSymbol state (tail x:xs) contenu
    | otherwise = parseSymbolCodeblock state (x:xs) contenu

parseSymbolCodeblock :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolCodeblock _ [] _ = Left "Error: Missing symbol in codeblock, list or link in text"
parseSymbolCodeblock state (x:xs) contenu
    | head x == '[' && last state == "beforeCodeblock" = parseSymbol (state ++ ["codeblock"]) (tail x:xs) contenu
    | head x == '[' && last state == "codeblock" = parseSymbol (state ++ ["incodeblock"]) (tail x:xs) contenu
    | head x == '[' && last state == "beforeList" = parseSymbol (state ++ ["list"]) (tail x:xs) contenu
    | head x == '[' && last state == "list" = parseSymbol (state ++ ["inlist"]) (tail x:xs) contenu
    | head x == '[' && last state == "inlink" = parseSymbol (state ++ ["contentlink"]) (tail x:xs) contenu
    | head x == '[' && last state == "inimage" = parseSymbol (state ++ ["altimage"]) (tail x:xs) contenu
    | otherwise = Left "Error: Missing symbol"

parseSymbol :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbol _ [] contenu = Left "Error: Missing } in text"
parseSymbol state ([]:xs) contenu 
    | xs == [] = Right contenu
    | otherwise = parseText state xs contenu
parseSymbol state (x:xs) contenu
    | head x == '}' || head x == ']' = parseSymbol (init state) (tail x:xs) contenu
    | head x == '[' && last state == "section" = parseSymbol (state ++ ["contenu"]) (tail x:xs) contenu
    | head x == '[' && last state == "contenu" = parseSymbol (state ++ ["paragraph"]) (tail x:xs) (appendPContent state (PParagraphContent (PParagraph [])) contenu)
    | head x == '{' && last state == "contenu" = parseSymbol (state ++ ["?"]) (tail x:xs) contenu
    | notBracketChar x = parseSymbol state (tail x:xs) contenu
    | otherwise = parseSymbolParagraph state (x:xs) contenu

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

parseBody :: String -> IO (Either String [PContent])
parseBody file_content = return $ enterInSection [] (strToWordArray "\"" "" file_content) []
