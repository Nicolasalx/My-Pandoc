--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
--

module ParseJson.ParseBody (parseBody) where
import Content (PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..))
import ParsingLib.Lib (strToWordArray, strcmp, nth)
import ParseJson.ParseFunction (notBracketChar, appendPContent, initPContent, lastPContent, appendCodeBlock)
import Debug.Trace

addTitle :: String -> PContent -> PContent
addTitle str (PSectionContent (PSection {title = _, section_content = content})) = PSectionContent (PSection {title = str, section_content = content})

parseTitle :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseTitle state (x:xs) content = parseSymbol (state ++ ["section"]) xs (appendPContent state (addTitle x (lastPContent state content)) ((initPContent state content)))

-- Parsing paragraph

addParagraph :: String -> String -> PContent -> PContent
addParagraph "text" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PTextParagraph (PText [PString str])]
addParagraph "bold" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PTextParagraph (PText [PBoldText (PBold [PString str])])]
addParagraph "italic" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PTextParagraph (PText [PItalicText (PItalic [PString str])])]
addParagraph "code" str (PParagraphContent (PParagraph list)) = PParagraphContent $ PParagraph $ list ++ [PTextParagraph (PText [PCodeText (PCode [PString str])])]

parseParagraph :: String -> [String] -> [String] -> [PContent] -> Either String [PContent]
parseParagraph typeStr state (x:xs) content = parseSymbol state xs (appendPContent state (addParagraph typeStr x (lastPContent state content)) ((initPContent state content)))
    
-- check quel est le type du text et call la bonne fonction

parseText :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseText _ [] _ = Left "Error: Missing ] in text"
parseText state (x:xs) content
    | s == "paragraph" = parseParagraph "text" state (x:xs) content
    | s == "?" && x == "section" = parseSymbol ((init state) ++ ["beforeSection"]) xs (appendPContent state (PSectionContent (PSection {title = "", section_content = []})) content)
    | s == "beforeSection" && x == "title" = parseTitle state (nth 1 xs) content
    | s == "section" && x == "content" = parseSymbol state xs content
    | s == "?" && x == "codeblock" = trace ("\nstate = " ++ show state ++ "\n") parseSymbol ((init state) ++ ["beforeCodeblock"]) xs (appendCodeBlock state (PCodeBlockContent (PCodeBlock [])) content)
    | s == "?" && (x == "bold" || x == "italic" || x == "code") = parseSymbol ((init state) ++ [x])  xs content
    | (s == "bold" || s == "italic" || s == "code") = parseParagraph s state (x:xs) content
    | otherwise = Right content
    where 
        s = last state
        
-- rempli la list d'état avec le type de contenu puis appelle parseText

parseSymbolParagraph :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolParagraph _ [] _ = Left "Error: Missing ] in symbol"
parseSymbolParagraph state (x:xs) content
    | head x == ']' && last state == "paragraph" = parseSymbol (init state) (tail x:xs) content
    | head x == '{' && last state == "paragraph" = parseSymbol (state ++ ["?"]) (tail x:xs) content
    | otherwise = parseSymbolSection state (x:xs) content

parseSymbolSection :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolSection _ [] _ = Left "Error: Missing ] in symbol"
parseSymbolSection state (x:xs) content
    | head x == '{' && last state == "beforeSection" = parseSymbol state (tail x:xs) content
    | otherwise = parseSymbolCodeblock state (x:xs) content

parseSymbolCodeblock :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolCodeblock _ [] _ = Left "Error: Missing ] in symbol"
parseSymbolCodeblock state (x:xs) content
    | head x == '[' && last state == "beforeCodeblock" = parseSymbol (state ++ ["codeblock"]) (tail x:xs) content
    | otherwise = Right content

parseSymbol :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbol _ [] content = Right content
parseSymbol state ([]:xs) content = parseText state xs content
parseSymbol state (x:xs) content
    | head x == '}' = parseSymbol (init state) (tail x:xs) content
    | head x == '[' && last state == "section" = parseSymbol (state ++ ["content"]) (tail x:xs) content
    | head x == '[' && last state == "content" = parseSymbol (state ++ ["paragraph"]) (tail x:xs) (appendPContent state (PParagraphContent (PParagraph [])) content)
    | head x == '{' && last state == "content" = parseSymbol (state ++ ["?"]) (tail x:xs) content
    | notBracketChar x = parseSymbol state (tail x:xs) content
    | otherwise = parseSymbolParagraph state (x:xs) content

-- Parsing de la base

initDataParsing :: [String] -> [String]
initDataParsing x = x ++ ["section"]

parseHeader :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseHeader _ [] _ = Left "Error: Missing } in header"
parseHeader dataParsing (x:xs) content
    | '}' `elem` x = parseBaseLoop dataParsing xs content
    | otherwise = parseHeader dataParsing xs content

parseBaseLoop :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseBaseLoop _ [] content = Right content
parseBaseLoop dataParsing (x:xs) content 
    | strcmp "header" x = parseHeader dataParsing xs content
    | strcmp "body" x = parseSymbol (initDataParsing dataParsing) xs content
    | otherwise = Right content

enterInSection :: [String] -> [String] -> [PContent] -> Either String [PContent]
enterInSection _ [] content = Right content
enterInSection dataParsing (x:xs) content
    | '{' `elem` x = parseBaseLoop dataParsing xs content
    | otherwise = Left "Error: Missing { in section"

parseBody :: String -> IO (Either String [PContent])
parseBody file_content = return $ enterInSection [] (strToWordArray "\"" "" file_content) []
