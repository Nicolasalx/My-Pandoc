module ParseXml.ParseBody (parseBody) where

import Content (PBody(..), PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PImage(..), PLink(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import ParsingLib.Lib (strToWordArray)
import Debug.Trace

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
                        trimmedParagraphLines = init (init paragraphLines)
                    in formatType trimmedParagraphLines ++ parseSectionsWithLevel parent_section restLines
               | otherwise ->
                    parseSectionsWithLevel parent_section linesContent

formatType :: [String] -> [PContent]
formatType paragraphLines = do
    trace ("paragraphLines: " ++ show paragraphLines) $ concatMap parseLine paragraphLines
  where
    parseLine line = parseWords "" (strToWordArray "<>/=" "" line) False False False createParagraph

    parseWords :: String -> [String] -> Bool -> Bool -> Bool -> PContent -> [PContent]
    parseWords _ [] _ _ _ _ = []
    parseWords prev (word:rest) inBold inItalic inCode new_paragraph
        | word == "bold" && not inBold = addBoldAndText rest
        | word == "italic" && not inItalic = addItalicAndText rest
        | word == "code" && not inCode = addCodeAndText rest
        | isFormattingTag word = parseWords word rest (word == "bold") (word == "italic") (word == "code") new_paragraph
        | otherwise = case prev of
            "paragraph" -> currentContent ++ parseWords word rest inBold inItalic inCode new_paragraph
            "bold" -> addBoldOrText word : parseWords "" rest inBold inItalic inCode new_paragraph
            "italic" -> addItalicOrText word : parseWords "" rest inBold inItalic inCode new_paragraph
            "code" -> addCodeOrText word : parseWords "" rest inBold inItalic inCode new_paragraph
            _ -> parseWords word rest inBold inItalic inCode new_paragraph
      where
        currentContent = [addParagraph "text" word new_paragraph]
        addBoldOrText w = if inBold then addParagraph "text" w new_paragraph else addParagraph "bold" w new_paragraph
        addBoldAndText (nextWord:remainingWords) = addParagraph "bold" nextWord new_paragraph : parseWords "" (init remainingWords) True inItalic inCode new_paragraph
        addItalicOrText w = if inItalic then addParagraph "text" w new_paragraph else addParagraph "italic" w new_paragraph
        addItalicAndText (nextWord:remainingWords) = addParagraph "italic" nextWord new_paragraph : parseWords "" (init remainingWords) inBold True inCode new_paragraph
        addCodeOrText w = if inCode then addParagraph "text" w new_paragraph else addParagraph "code" w new_paragraph
        addCodeAndText (nextWord:remainingWords) = addParagraph "code" nextWord new_paragraph : parseWords "" (init remainingWords) inBold inItalic True new_paragraph

    isFormattingTag :: String -> Bool
    isFormattingTag tag = tag `elem` ["bold", "italic", "code"]

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

-- addParagraph :: String -> String -> PContent
-- addParagraph "text" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PString str])]
-- addParagraph "bold" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PBoldText (PBold [PString str])])]
-- addParagraph "italic" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PItalicText (PItalic [PString str])])]
-- addParagraph "code" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PCodeText (PCode [PString str])])]
-- addParagraph "link" _ = PParagraphContent $ PParagraph [PLinkParagraph (PLink {link_url = "", content = PText []})]
-- addParagraph "image" _ = PParagraphContent $ PParagraph [PImageParagraph (PImage {image_url = "", alt = PText []})]
-- addParagraph _ _ = PParagraphContent $ PParagraph []

createParagraph :: PContent
createParagraph = PParagraphContent $ PParagraph []

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
    | "<paragraph>" `isPrefixOf` strip str = trace "Stripping opening paragraph tag" $ strip (drop (length "<paragraph>") str)
    | "</paragraph>" `isSuffixOf` strip str = trace "Stripping closing paragraph tag" $ strip (reverse $ drop (length "</paragraph>") $ reverse $ strip str)
    | "<codeblock>" `isPrefixOf` strip str = trace "Stripping opening codeblock tag" $ strip (drop (length "<codeblock>") str)
    | "<list>" `isPrefixOf` strip str = trace "Stripping opening list tag" $ strip (drop (length "<list>") str)
    | otherwise = trace "No tags to strip" $ strip str


createSection :: String -> [PContent] -> PContent
createSection title content =
    PSectionContent (PSection title content)

extractTitle :: String -> String
extractTitle line
    | "<section title=\"" `isPrefixOf` strip line = cleanTitle $ dropWhile (/= '"') (dropWhile (/= '\"') line)
    | otherwise = ""
    where
        cleanTitle = init . init . tail . strip
