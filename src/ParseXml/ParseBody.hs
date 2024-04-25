module ParseXml.ParseBody (parseBody) where

import Content (PBody(..), PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PImage(..), PLink(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import ParsingLib.Lib (strToWordArray)

parseBody :: String -> IO (Either String PBody)
parseBody file_content = do
    let linesContent = lines file_content
        sections = parseSections linesContent
    return $ Right (PBody sections)

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

parseSections :: [String] -> [PContent]
parseSections = parseSectionsWithLevel []

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

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

removeParagraphEnd :: String -> String
removeParagraphEnd str
    | "</paragraph>" `isSuffixOf` str = take (length str - length "</paragraph>") str
    | otherwise = str
    
formatType :: [String] -> [PContent]
formatType paragraphLines = concatMap parseLine paragraphLines
  where
    parseLine line = parseWords "" (strToWordArray "<>/" "" line) False False False

    parseWords :: String -> [String] -> Bool -> Bool -> Bool -> [PContent]
    parseWords _ [] _ _ _ = []
    parseWords prev (word:rest) inBold inItalic inCode
        | word == "bold" && not inBold = addBoldAndText rest
        | word == "italic" && not inItalic = addItalicAndText rest
        | word == "code" && not inCode = addCodeAndText rest
        | isFormattingTag word = parseWords word rest (word == "bold") (word == "italic") (word == "code")
        | otherwise = case prev of
            "paragraph" -> currentContent ++ parseWords word rest inBold inItalic inCode
            "bold" -> addBoldOrText word : parseWords "" rest inBold inItalic inCode
            "italic" -> addItalicOrText word : parseWords "" rest inBold inItalic inCode
            "code" -> addCodeOrText word : parseWords "" rest inBold inItalic inCode
            _ -> parseWords word rest inBold inItalic inCode
      where
        currentContent = [addParagraph "text" word]
        addBoldOrText w = if inBold then addParagraph "text" w else addBold w
        addBoldAndText (nextWord:remainingWords) = addBold nextWord : parseWords "" (init remainingWords) True inItalic inCode
        addItalicOrText w = if inItalic then addParagraph "text" w else addItalic w
        addItalicAndText (nextWord:remainingWords) = addItalic nextWord : parseWords "" (init remainingWords) inBold True inCode
        addCodeOrText w = if inCode then addParagraph "text" w else addCode w
        addCodeAndText (nextWord:remainingWords) = addCode nextWord : parseWords "" (init remainingWords) inBold inItalic True

    isFormattingTag :: String -> Bool
    isFormattingTag tag = tag `elem` ["bold", "italic", "code"]

    addBold w = addParagraph "bold" w
    addItalic w = addParagraph "italic" w
    addCode w = addParagraph "code" w



addParagraph :: String -> String -> PContent
addParagraph "text" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PString str])]
addParagraph "bold" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PBoldText (PBold [PString str])])]
addParagraph "italic" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PItalicText (PItalic [PString str])])]
addParagraph "code" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PCodeText (PCode [PString str])])]
addParagraph "link" _ = PParagraphContent $ PParagraph [PLinkParagraph (PLink {link_url = "", content = PText []})]
addParagraph "image" _ = PParagraphContent $ PParagraph [PImageParagraph (PImage {image_url = "", alt = PText []})]
addParagraph _ _ = PParagraphContent $ PParagraph []

isCodeBlockEnd :: String -> Bool
isCodeBlockEnd line = "</codeblock>" `isSuffixOf` strip line

isListEnd :: String -> Bool
isListEnd line = "</list>" `isSuffixOf` strip line

isSectionEnd :: String -> Bool
isSectionEnd = (== "</section>") 

isParagraphEnd :: String -> Bool
isParagraphEnd = (== "</paragraph>")

stripTags :: String -> String
stripTags str
    | "<paragraph>" `isPrefixOf` strip str = strip (drop (length "<paragraph>") str)
    | "</paragraph>" `isSuffixOf` strip str = strip (reverse $ drop (length "</paragraph>") $ reverse $ strip str)
    | "<codeblock>" `isPrefixOf` strip str = strip (drop (length "<codeblock>") str)
    | "<list>" `isPrefixOf` strip str = strip (drop (length "<list>") str)
    | otherwise = strip str

createSection :: String -> [PContent] -> PContent
createSection title content =
    PSectionContent (PSection title content)

extractTitle :: String -> String
extractTitle line
    | "<section title=\"" `isPrefixOf` strip line = cleanTitle $ dropWhile (/= '"') (dropWhile (/= '\"') line)
    | otherwise = ""
    where
        cleanTitle = init . init . tail . strip
