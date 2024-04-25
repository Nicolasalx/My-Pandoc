module ParseXml.Paragraph (formatType, strip, removeParagraphEnd, stripTags, isParagraphEnd) where

import Content (PBody(..), PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PImage(..), PLink(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import ParsingLib.Lib (strToWordArray)
import Debug.Trace

formatType :: [String] -> [PContent]
formatType paragraphLines = do
    trace ("paragraphLines: " ++ show paragraphLines) $ concatMap parseLine paragraphLines
  where
    parseLine line = createParagraph : parseWords "" (strToWordArray "<>/=" "" line) False False False

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
    addBoldOrText w = if inBold then addParagraph "text" w  else addParagraph "bold" w 
    addBoldAndText (nextWord:remainingWords) = addParagraph "bold" nextWord  : parseWords "" (init remainingWords) True inItalic inCode 
    addItalicOrText w = if inItalic then addParagraph "text" w  else addParagraph "italic" w 
    addItalicAndText (nextWord:remainingWords) = addParagraph "italic" nextWord  : parseWords "" (init remainingWords) inBold True inCode 
    addCodeOrText w = if inCode then addParagraph "text" w  else addParagraph "code" w 
    addCodeAndText (nextWord:remainingWords) = addParagraph "code" nextWord  : parseWords "" (init remainingWords) inBold inItalic True

isFormattingTag :: String -> Bool
isFormattingTag tag = tag `elem` ["bold", "italic", "code"]

addParagraph :: String -> String -> PContent
addParagraph "text" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PString str])]
addParagraph "bold" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PBoldText (PBold [PString str])])]
addParagraph "italic" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PItalicText (PItalic [PString str])])]
addParagraph "code" str = PParagraphContent $ PParagraph [PTextParagraph (PText [PCodeText (PCode [PString str])])]
addParagraph "link" _ = PParagraphContent $ PParagraph [PLinkParagraph (PLink {link_url = "", content = PText []})]
addParagraph "image" _ = PParagraphContent $ PParagraph [PImageParagraph (PImage {image_url = "", alt = PText []})]
addParagraph _ _ = PParagraphContent $ PParagraph []

createParagraph :: PContent
createParagraph = PParagraphContent $ PParagraph []

isParagraphEnd :: String -> Bool
isParagraphEnd = (== "</paragraph>")

removeParagraphEnd :: String -> String
removeParagraphEnd str
    | "</paragraph>" `isSuffixOf` str = take (length str - length "</paragraph>") str
    | otherwise = str

stripTags :: String -> String
stripTags str
    | "<paragraph>" `isPrefixOf` strip str = trace "Stripping opening paragraph tag" $ strip (drop (length "<paragraph>") str)
    | "</paragraph>" `isSuffixOf` strip str = trace "Stripping closing paragraph tag" $ strip (reverse $ drop (length "</paragraph>") $ reverse $ strip str)
    | "<codeblock>" `isPrefixOf` strip str = trace "Stripping opening codeblock tag" $ strip (drop (length "<codeblock>") str)
    | "<list>" `isPrefixOf` strip str = trace "Stripping opening list tag" $ strip (drop (length "<list>") str)
    | otherwise = trace "No tags to strip" $ strip str

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace