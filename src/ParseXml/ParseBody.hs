module ParseXml.ParseBody (parseBody) where

import Content (
    PHeader(..), PBody(..), PContent(..), PText(..), PTextType(..),
    PBold(..), PItalic(..), PCode(..), PLink(..), PImage(..),
    PParagraph(..), PParagraphType(..), PSection(..), PCodeBlock(..),
    PList(..), PItem(..), PItemType(..)
    )
import ParseXml.DataStructXml (DataParsing(..), initializeDataParsing)
import ParsingLib.Lib (strcmp, addParagraph, cleanLine)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace, traceIO, traceM)
import Data.Char (isSpace)

parseBody :: String -> IO (Either String PBody)
parseBody file_content = do
    let linesContent = lines file_content
    traceIO $ "Lines Content: " ++ show linesContent
    case analyzeContent ["paragraph"] linesContent [] of
        Left err -> return $ Left err
        Right content -> do
            traceIO $ "Content Analyzed: " ++ show content
            return $ Right (PBody content)

analyzeContent :: [String] -> [String] -> [PContent] -> Either String [PContent]
analyzeContent _ [] content = do
    traceM "Reached end of document"
    return content
analyzeContent state (x:xs) content
        | "<paragraph>" `isPrefixOf` cleanX && last state == "paragraph" = do
            let paragraphContent = getContentBetweenTags "<paragraph>" x
            traceM $ "Adding paragraph content: " ++ paragraphContent
            analyzeContent state xs (addParagraphContent paragraphContent : content)
        | "<section title=" `isPrefixOf` cleanX = do
            let (sectionContent, remainingLines) = extractSectionContent xs
            subcontent <- analyzeContent (last state : state) remainingLines []
            traceM $ "Adding section content: " ++ show sectionContent
            analyzeContent state xs (addSectionContent cleanX sectionContent : subcontent)
        | "<codeblock>" `isPrefixOf` cleanX && last state == "paragraph" = do
            let codeBlockContent = getContentBetweenTags "<codeblock>" x
            traceM $ "Adding code block content: " ++ codeBlockContent
            analyzeContent state xs (addCodeBlock [codeBlockContent] : content)
        | "<list>" `isPrefixOf` cleanX && last state == "paragraph" = do
            traceM "Adding list content"
            analyzeContent state xs (addList [] : content)
        | "</section>" `isPrefixOf` cleanX = case state of
                                            "section":restState -> do
                                                let sectionContent = finalizeSectionContent (head content)
                                                traceM $ "Finalizing section content: " ++ show sectionContent
                                                analyzeContent restState xs (sectionContent : tail content)
                                            _ -> Left "Unexpected end of section tag"
        | otherwise = do
            traceM $ "Ignoring unrecognized content: " ++ x
            analyzeContent state xs content
    where cleanX = dropWhile isSpace x
    
addParagraphContent :: String -> PContent
addParagraphContent x = PParagraphContent (PParagraph [PTextParagraph (PText [PString x])])

addSectionContent :: String -> [PContent] -> PContent
addSectionContent x subcontent = PSectionContent (PSection (getContentBetweenTags "<section>" x) subcontent)

finalizeSectionContent :: PContent -> PContent
finalizeSectionContent (PSectionContent (PSection title subcontent)) = PSectionContent (PSection title (reverse subcontent))
finalizeSectionContent _ = error "finalizeSectionContent: not a section content"

getContentBetweenTags :: String -> String -> String
getContentBetweenTags openTag str =
    let tagLength = length openTag
        strWithoutSpaces = dropWhile isSpace str
        contentStartIdx = tagLength
        contentEndIdx = length strWithoutSpaces - (tagLength + length ("</" ++ takeWhile (/='>') openTag))
    in take contentEndIdx $ drop contentStartIdx strWithoutSpaces

addCodeBlock :: [String] -> PContent
addCodeBlock codeLines = PCodeBlockContent (PCodeBlock codeLines)

addList :: [PItem] -> PContent
addList items = PListContent (PList items)

extractSectionContent :: [String] -> ([PContent], [String])
extractSectionContent lines =
    let (sectionLines, rest) = break ("</section>" `isPrefixOf`) lines
        title = extractSectionTitle (head sectionLines)
    in (mapMaybe extractParagraph (tail sectionLines), rest)
    where
        extractSectionTitle :: String -> String
        extractSectionTitle line =
            let titleStart = dropWhile (/= '"') line
                titleEnd = dropWhile (== ' ') $ tail titleStart
                extractedTitle = takeWhile (/= '"') titleEnd
            in trace ("Extracted section title: " ++ extractedTitle) extractedTitle

extractParagraph :: String -> Maybe PContent
extractParagraph line
    | "<paragraph>" `isPrefixOf` line = Just $ addParagraphContent (getContentBetweenTags "<paragraph>" line)
    | "</paragraph>" `isPrefixOf` line = Just $ addParagraphContent (getContentBetweenTags "</paragraph>" line)
    | otherwise = Nothing
