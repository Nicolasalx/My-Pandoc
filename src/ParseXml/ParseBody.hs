module ParseXml.ParseBody (parseBody) where

import Content (PContent(..), PParagraph(..), PParagraphType(..), PBody(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PLink(..), PImage(..))
import ParsingLib.Lib (strcmp, addParagraph, cleanLine)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace, traceIO, traceM)
import Data.Char (isSpace)

parseBody :: String -> IO (Either String PBody)
parseBody file_content = do
    let linesContent = lines file_content
    -- traceIO $ "Lines Content: " ++ show linesContent
    case analyzeContent ["paragraph"] linesContent [] of
        Left err -> return $ Left err
        Right content -> do
            -- traceIO $ "Content Analyzed: " ++ show content
            return $ Right (PBody content)

analyzeContent :: [String] -> [String] -> [PContent] -> Either String [PContent]
analyzeContent _ [] content = Right (reverse content)
analyzeContent state (x:xs) content
    | strcmp "<paragraph>" cleanSpace && last state == "paragraph" =
        let paragraphContent = getContentBetweenTags "<paragraph>" x
        in analyzeContent state xs (addParagraphContent paragraphContent : content)
    | strcmp "<section title=" cleanSpace =
        let sectionTitle = extractSectionTitle x
        in analyzeContent ("section":state) xs (PSectionContent (PSection sectionTitle []) : content)
    | strcmp "<link url=" cleanSpace && last state == "paragraph" =
        let (url, linkContent) = extractLinkContent x
        in analyzeContent state xs (addLinkContent url linkContent : content)
    | strcmp "<image url=" cleanSpace && last state == "paragraph" =
        let (url, imageContent) = extractImageContent x
        in analyzeContent state xs (addImageContent url imageContent : content)
    | otherwise =
        analyzeContent state xs content
    where cleanSpace = dropWhile isSpace x
    
extractContentInQuotes :: String -> Maybe String
extractContentInQuotes ('"':rest) = Just (takeWhile (/= '"') rest)
extractContentInQuotes _ = Nothing

extractSectionTitle :: String -> String
extractSectionTitle line = maybe "" id (extractContentInQuotes (drop 15 cleanLine))
    where
        cleanLine = cleanSpace line
        cleanSpace = dropWhile isSpace

addParagraphContent :: String -> PContent
addParagraphContent x = PParagraphContent (PParagraph [PTextParagraph (PText [PString x])])

getContentBetweenTags :: String -> String -> String
getContentBetweenTags openTag str = take contentEndIdx (drop contentStartIdx strWithoutSpaces)
    where
        tagLength = length openTag
        strWithoutSpaces = dropWhile isSpace str
        contentStartIdx = tagLength
        contentEndIdx = length strWithoutSpaces - (tagLength + length ("</" ++ takeWhile (/='>') openTag))

addFormattedContent :: String -> String -> String -> (PTextType -> PTextType) -> PContent
addFormattedContent "" "" "" _ = PParagraphContent (PParagraph [])
addFormattedContent before content after formatConstructor =
    let textBefore = if null before then [] else [PString before]
        textAfter = if null after then [] else [PString after]
        formattedText = if null content then [] else [formatConstructor (PString content)]
    in PParagraphContent (PParagraph [PTextParagraph (PText (textBefore ++ formattedText ++ textAfter))])

addItalicText :: PTextType -> PTextType
addItalicText = PItalicText . PItalic . return

addBoldText :: PTextType -> PTextType
addBoldText = PBoldText . PBold . return

addCodeText :: PTextType -> PTextType
addCodeText = PCodeText . PCode . return

extractParagraph :: String -> Maybe PContent
extractParagraph line
    | strcmp "<paragraph>" line = Just $ addParagraphContent (getContentBetweenTags "<paragraph>" line)
    | strcmp "</paragraph>" line = Just $ addParagraphContent (getContentBetweenTags "</paragraph>" line)
    | Just (before, italic, after) <- extractItalic line = Just $ addFormattedContent before italic after addItalicText
    | Just (before, bold, after) <- extractBold line = Just $ addFormattedContent before bold after addBoldText
    | Just (before, code, after) <- extractCode line = Just $ addFormattedContent before code after addCodeText
    | otherwise = Nothing

extractItalic :: String -> Maybe (String, String, String)
extractItalic line
    | strcmp "<italic>" line && strcmp "</italic>" line = Just (before, italic, after)
    | otherwise = Nothing
    where
        before = takeWhile (/= '<') line
        italic = getContentBetweenTags "<italic>" line
        after = drop (length italic + length "</italic>") $ dropWhile (/= '>') line

extractBold :: String -> Maybe (String, String, String)
extractBold line
    | strcmp "<bold>" line && strcmp "</bold>" line = Just (before, bold, after)
    | otherwise = Nothing
    where
        before = takeWhile (/= '<') line
        bold = getContentBetweenTags "<bold>" line
        after = drop (length bold + length "</bold>") $ dropWhile (/= '>') line

extractCode :: String -> Maybe (String, String, String)
extractCode line
    | strcmp "<code>" line && strcmp "</code>" line = Just (before, code, after)
    | otherwise = Nothing
    where
        before = takeWhile (/= '<') line
        code = getContentBetweenTags "<code>" line
        after = drop (length code + length "</code>") $ dropWhile (/= '>') line

addLinkContent :: String -> String -> PContent
addLinkContent url linkContent = PParagraphContent (PParagraph [PLinkParagraph (PLink url (PText [PString linkContent]))])

extractLinkContent :: String -> (String, String)
extractLinkContent line = (url, content)
    where
        (urlStart, rest) = break (== '"') (dropWhile (/= '"') line)
        url = drop 1 urlStart
        content = getContentBetweenTags "<link>" rest

addImageContent :: String -> String -> PContent
addImageContent url altText = PParagraphContent (PParagraph [PImageParagraph (PImage url (PText [PString altText]))])

extractImageContent :: String -> (String, String)
extractImageContent line = (url, altText)
    where
        (urlStart, rest) = break (== '"') (dropWhile (/= '"') line)
        url = drop 1 urlStart
        altText = getContentBetweenTags "<image>" rest

