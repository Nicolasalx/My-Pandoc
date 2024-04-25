module ParseXml.Sections (parseSections) where

import Content (PBody(..), PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PImage(..), PLink(..)) 
import Data.List (isPrefixOf, isSuffixOf)
import ParseXml.List (parseList)
import ParseXml.CodeBlock (parseCodeBlock)
import ParseXml.Paragraph (strip, formatType, isParagraphEnd)

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

createSection :: String -> [PContent] -> PContent
createSection title content =
    PSectionContent (PSection title content)

extractTitle :: String -> String
extractTitle line
    | "<section title=\"" `isPrefixOf` strip line = cleanTitle $ dropWhile (/= '"') (dropWhile (/= '\"') line)
    | otherwise = ""
    where
        cleanTitle = init . init . tail . strip

parseSections :: [String] -> [PContent]
parseSections = parseSectionsWithLevel []

isCodeBlockEnd :: String -> Bool
isCodeBlockEnd line = "</codeblock>" `isSuffixOf` strip line

isListEnd :: String -> Bool
isListEnd line = "</list>" `isSuffixOf` strip line

isSectionEnd :: String -> Bool
isSectionEnd = (== "</section>") 