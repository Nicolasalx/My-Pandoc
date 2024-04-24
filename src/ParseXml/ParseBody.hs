module ParseXml.ParseBody (parseBody) where

import Content (PBody(..), PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, span, isSuffixOf, isInfixOf)
import ParsingLib.Lib (strToWordArray)
import Debug.Trace (trace)

parseBody :: String -> IO (Either String PBody)
parseBody file_content = do
    let linesContent = lines file_content
        sections = parseSections linesContent
    return $ Right (PBody sections)

parseSections :: [String] -> [PContent]
parseSections = parseSectionsWithLevel []

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

parseSectionsWithLevel :: [String] -> [String] -> [PContent]
parseSectionsWithLevel _ [] = []
parseSectionsWithLevel parent_section (line:linesContent) =
    let line_without_space = strip line
    in
        case () of
            () | isPrefixOf "<section title=\"" line_without_space ->
                    let title = extractTitle line_without_space
                        (sectionLines, restLines) = span (not . isSectionEnd) linesContent
                        sectionContent = parseSectionsWithLevel (title:parent_section) sectionLines
                        section = createSection title sectionContent
                    in [section] ++ parseSectionsWithLevel parent_section restLines
               | isPrefixOf "<codeblock>" line_without_space ->
                    let (codeLines, restLines) = span (not . isCodeBlockEnd) (line_without_space:linesContent)
                    in parseCodeBlock codeLines ++ parseSectionsWithLevel parent_section restLines
               | isPrefixOf "<list>" line_without_space ->
                    let (listLines, restLines) = span (not . isListEnd) (line_without_space:linesContent)
                    in parseList listLines ++ parseSectionsWithLevel parent_section restLines
               | isPrefixOf "<paragraph>" line_without_space ->
                    let (paragraphLines, restLines) = span (not . isParagraphEnd) (line_without_space:linesContent)
                    in formatType paragraphLines ++ parseSectionsWithLevel parent_section restLines
               | otherwise ->
                    parseSectionsWithLevel parent_section linesContent

addCodeBlock :: String -> PContent -> PContent
addCodeBlock str (PCodeBlockContent (PCodeBlock list))
    | null cleanedStr = PCodeBlockContent (PCodeBlock list)
    | otherwise = PCodeBlockContent (PCodeBlock $ list ++ [cleanedStr])
  where
    cleanedStr = stripTags $ strip $ removeParagraphEnd str


parseCodeBlock :: [String] -> [PContent]
parseCodeBlock codeLines =
    let codeBlockContent = PCodeBlockContent (PCodeBlock [])
        updatedCodeBlockContent = foldr addCodeBlock codeBlockContent codeLines
    in case updatedCodeBlockContent of
        PCodeBlockContent (PCodeBlock content) -> [PCodeBlockContent (PCodeBlock (filter (not . null) content))]
        _ -> [updatedCodeBlockContent]

addList :: String -> PContent -> PContent
addList str (PListContent (PList list))
    | null cleanedStr = PListContent (PList list)
    | otherwise = PListContent $ PList $ newItem : list
    where
        newItem = PItem [(PParagraphItem (PParagraph [PTextParagraph (PText [PString cleanedStr])]))]
        cleanedStr = stripTags $ strip $ removeParagraphEnd str

parseList :: [String] -> [PContent]
parseList listLines =
    let listContent = PListContent (PList [])
        updatedListContent = foldr addList listContent listLines
    in case updatedListContent of
        PListContent (PList content) -> [PListContent (PList (filterItems content))]
        _ -> [updatedListContent]

filterItems :: [PItem] -> [PItem]
filterItems = filter (not . isEmptyItem)

isEmptyItem :: PItem -> Bool
isEmptyItem (PItem []) = True
isEmptyItem _ = False

parseParagraph :: String -> [String] -> [PContent]
parseParagraph _ [] = []
parseParagraph format lines =
    case extractContent format lines of
        Just (content, restLines) ->
            let paragraphContent = case format of
                    "bold" -> PParagraph [PTextParagraph $ PText [PBoldText (PBold [PString content])]]
                    "italic" -> PParagraph [PTextParagraph $ PText [PItalicText (PItalic [PString content])]]
                    "code" -> PParagraph [PTextParagraph $ PText [PCodeText (PCode [PString content])]]
                    _ -> PParagraph [PTextParagraph $ PText [PString content]]
            in PParagraphContent paragraphContent : parseParagraph format restLines
        Nothing -> []

removeParagraphEnd :: String -> String
removeParagraphEnd str
    | "</paragraph>" `isSuffixOf` str = take (length str - length "</paragraph>") str
    | otherwise = str
    
formatType :: [String] -> [PContent]
formatType paragraphLines = concatMap parseLine paragraphLines
  where
    parseLine line
      | "<bold>" `isInfixOf` line = parseParagraph "bold" [line]
      | "<italic>" `isInfixOf` line = parseParagraph "italic" [line]
      | "<code>" `isInfixOf` line = parseParagraph "code" [line]
      | otherwise = parseParagraph "text" [line]

isCodeBlockEnd :: String -> Bool
isCodeBlockEnd line = "</codeblock>" `isSuffixOf` strip line

isListEnd :: String -> Bool
isListEnd line = "</list>" `isSuffixOf` strip line

isSectionEnd :: String -> Bool
isSectionEnd = (== "</section>") 

isParagraphEnd :: String -> Bool
isParagraphEnd = (== "</paragraph>")

extractContent :: String -> [String] -> Maybe (String, [String])
extractContent _ [] = Nothing
extractContent format (line:rest)
    | "<paragraph>" `isPrefixOf` strip line = 
        let line_without_space = strip line
            contentWithEnd = stripTags line_without_space
        in case strip contentWithEnd of
            "</paragraph>" -> Just ("", rest)
            _ -> Just (extractParagraphContent [contentWithEnd], rest)
    | otherwise = extractContent format rest

stripTags :: String -> String
stripTags str
    | "<paragraph>" `isPrefixOf` strip str = strip (drop (length "<paragraph>") str)
    | "</paragraph>" `isSuffixOf` strip str = strip (reverse $ drop (length "</paragraph>") $ reverse $ strip str)
    | "<codeblock>" `isPrefixOf` strip str = strip (drop (length "<codeblock>") str)
    | "<list>" `isPrefixOf` strip str = strip (drop (length "<list>") str)
    | otherwise = strip str

extractParagraphContent :: [String] -> String
extractParagraphContent paragraphLines =
    let content = concatMap (dropWhile isSpace . stripTags) paragraphLines
    in content

createSection :: String -> [PContent] -> PContent
createSection title content =
    PSectionContent (PSection title content)

extractTitle :: String -> String
extractTitle line
    | "<section title=\"" `isPrefixOf` strip line = cleanTitle $ dropWhile (/= '"') (dropWhile (/= '\"') line)
    | otherwise = ""
    where
        cleanTitle = init . init . tail . strip
