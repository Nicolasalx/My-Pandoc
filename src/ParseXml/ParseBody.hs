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
import Debug.Trace (trace)
import Data.Char (isSpace)

parseBody :: String -> IO (Either String PBody)
parseBody file_content = do
    let linesContent = lines file_content
    case analyzeContent ["paragraph"] linesContent [] of
        Left err -> return $ Left err
        Right content -> return $ Right (PBody content)

getContentLinesBetweenTags :: String -> String -> [String] -> [String]
getContentLinesBetweenTags openTag closeTag lines =
    let bodyLines = dropWhile (not . isPrefixOf openTag) lines
    in trace ("Open tag found: " ++ show (not $ null bodyLines)) $
        case bodyLines of
            [] -> []
            (_ : rest) -> takeWhile (not . isPrefixOf closeTag) (mapMaybe cleanLine rest)

analyzeContent :: [String] -> [String] -> [PContent] -> Either String [PContent]
analyzeContent _ [] content = Right content
analyzeContent state (x:xs) content
        | "<paragraph>" `isPrefixOf` x && last state == "paragraph" = analyzeContent state xs (addParagraphContent x : content)
        | "<section>" `isPrefixOf` x = do
            let (sectionContent, remainingLines) = extractSectionContent xs
            subcontent <- analyzeContent (last state : state) remainingLines []
            analyzeContent state xs (addSectionContent x sectionContent : subcontent)
        | "<codeblock>" `isPrefixOf` x && last state == "paragraph" = analyzeContent state xs (addCodeBlock [getContentBetweenTags "<codeblock>" x] : content)
        | "<list>" `isPrefixOf` x && last state == "paragraph" = analyzeContent state xs (addList [] : content)
        | "</section>" `isPrefixOf` x = case state of
                                            "section":restState -> analyzeContent restState xs (finalizeSectionContent (head content) : tail content)
                                            _ -> Left "Unexpected end of section tag"
        | otherwise = analyzeContent state xs content

addParagraphContent :: String -> PContent
addParagraphContent x = PParagraphContent (PParagraph [PTextParagraph (PText [PString x])])

addSectionContent :: String -> [PContent] -> PContent
addSectionContent x subcontent = PSectionContent (PSection (getContentBetweenTags "<section>" x) subcontent)

finalizeSectionContent :: PContent -> PContent
finalizeSectionContent (PSectionContent (PSection title subcontent)) = PSectionContent (PSection title (reverse subcontent))
finalizeSectionContent _ = error "finalizeSectionContent: not a section content"

getContentBetweenTags :: String -> String -> String
getContentBetweenTags tag str = dropWhile isSpace $ drop (length tag) $ take (length str - length tag - 1) str

addCodeBlock :: [String] -> PContent
addCodeBlock codeLines = PCodeBlockContent (PCodeBlock codeLines)

addList :: [PItem] -> PContent
addList items = PListContent (PList items)

extractSectionContent :: [String] -> ([PContent], [String])
extractSectionContent lines =
    let (sectionLines, rest) = break ("</section>" `isPrefixOf`) lines
    in (mapMaybe extractParagraph sectionLines, rest)

extractParagraph :: String -> Maybe PContent
extractParagraph line
    | "<paragraph>" `isPrefixOf` line = Just $ addParagraphContent (getContentBetweenTags "<paragraph>" line)
    | "</paragraph>" `isPrefixOf` line = Just $ addParagraphContent (getContentBetweenTags "</paragraph>" line)
    | otherwise = Nothing