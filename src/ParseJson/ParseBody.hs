--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
--

module ParseJson.ParseBody (parseBody) where
import Content (PContent(..), PParagraph(..), PParagraphType(..), PText(..), PTextType(..), PSection(..))
import ParsingLib.AppendElemToDataStruct (addNewElemToContent)
import ParsingLib.Lib (strToWordArray, strcmp, searchSymbol)

-- fonction pour récupérer le PContent selon le state (endroit)
-- fonction pour ajouter un PContent vide selon le state (endroit)

addNewPContent :: [String] -> Bool -> PContent -> [PContent] -> [PContent]
addNewPContent [] _ newC content = content ++ [newC]
addNewPContent (x:xs) isBody newC content
    | x == "section" && isBody == True = (init content) ++ [PSectionContent ( PSection {title = "", section_content = (addNewPContent (xs) True newC [(last content)])})]
    | x == "section" && isBody == False = addNewPContent (xs) True newC content
    | otherwise = addNewPContent (xs) True newC content

-- Parsing paragraph

addParagraph :: String -> PContent -> PContent
addParagraph str (PParagraphContent (PParagraph list)) = PParagraphContent (PParagraph (list ++ [PTextParagraph (PText [PString str])]))

parseParagraph :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseParagraph state (x:xs) content = parseSymbol state xs False ((init content) ++ [addParagraph x (last content)])

-- check quel est le type du text et call la bonne fonction

parseText :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseText _ [] _ = Left "Error: Missing ] in text"
parseText state (x:xs) content
    | last state == "paragraph" = parseParagraph state (x:xs) (addNewPContent state False (PParagraphContent (PParagraph [])) content)

-- rempli la list d'état avec le type de contenu puis appelle parseText

parseSymbolParagraph :: [String] -> [String] -> [PContent] -> Either String [PContent]
parseSymbolParagraph _ [] _ = Left "Error: Missing ] in symbol"
parseSymbolParagraph state ([]:xs) content = parseText state xs content
parseSymbolParagraph state (x:xs) content
    | head x == ']' && last state == "paragraph" = parseSymbol (init state) (tail x:xs) False content
    | otherwise = Right content

parseSymbol :: [String] -> [String] -> Bool -> [PContent] -> Either String [PContent]
parseSymbol _ [] _ content = Right content
parseSymbol state ([]:xs) _ content = parseText state xs content
parseSymbol state (x:xs) isComma content
    | head x == ':' = parseSymbol state (tail x:xs) True content
    | head x == '[' && isComma && last state == "section" = parseSymbol (state ++ ["content"]) (tail x:xs) False content
    | head x == '[' && last state == "content" = parseSymbol (state ++ ["paragraph"]) (tail x:xs) False content
    | head x == ' ' || head x == '\n' || head x == ',' = parseSymbol state (tail x:xs) isComma content
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
    | strcmp "body" x = parseSymbol (initDataParsing dataParsing) xs False content
    | otherwise = Right content

enterInSection :: [String] -> [String] -> [PContent] -> Either String [PContent]
enterInSection _ [] content = Right content
enterInSection dataParsing (x:xs) content
    | '{' `elem` x = parseBaseLoop dataParsing xs content
    | otherwise = Left "Error: Missing { in section"

parseBody :: String -> IO (Either String [PContent])
parseBody file_content = return $ enterInSection [] (strToWordArray "\"" "" file_content) []
