{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
-}

module ParseXml.ParseBody (parseBody) where
import Content (PHeader(..), PBody(..),
    PContent(..),
    PText(..), PTextType(..), PBold(..), PItalic(..), PCode(..),
    PLink(..), PImage(..),
    PParagraph(..), PParagraphType(..), PSection(..), PCodeBlock(..),
    PList(..), PItem(..), PItemType(..))
import ParseXml.DataStructXml (DataParsing(..), initializeDataParsing)
import ParsingLib.Lib (strcmp, addParagraph)
import Data.List (isPrefixOf)

parseBody :: String -> IO (Either String PBody)
parseBody file_content = do
    let linesContent = lines file_content
    case analyzeContent ["paragraph"] linesContent [] of
        Left err -> return $ Left err
        Right content -> return $ Right (PBody content)

analyzeContent :: [String] -> [String] -> [PContent] -> Either String [PContent]
analyzeContent _ [] content = Right content
analyzeContent state (x:xs) content
    | "<paragraph>" `isPrefixOf` x && last state == "paragraph" = analyzeContent state xs (addParagraphContent x : init content)
    | "<section>" `isPrefixOf` x = analyzeContent (last state : state) xs (addSectionContent x [] : content)
    | "<codeblock>" `isPrefixOf` x && last state == "paragraph" = analyzeContent state xs (addCodeBlock [getContentBetweenTags "<codeblock>" x] : content)
    | "<list>" `isPrefixOf` x && last state == "paragraph" = analyzeContent state xs (addList [] : content)
    | "</section>" `isPrefixOf` x = case state of
                                        "section":restState -> analyzeContent restState xs (finalizeSectionContent (head content) : tail content)
                                        _ -> Left "Unexpected end of section tag"
    | otherwise = analyzeContent state xs content

addParagraphContent :: String -> PContent
addParagraphContent x = PParagraphContent (PParagraph [PTextParagraph (PText [PString (getContentBetweenTags "<paragraph>" x)])])

addSectionContent :: String -> [PContent] -> PContent
addSectionContent x subcontent = PSectionContent (PSection (getContentBetweenTags "<section>" x) subcontent)

finalizeSectionContent :: PContent -> PContent
finalizeSectionContent (PSectionContent (PSection title subcontent)) = PSectionContent (PSection title (reverse subcontent))
finalizeSectionContent _ = error "finalizeSectionContent: not a section content"

getContentBetweenTags :: String -> String -> String
getContentBetweenTags str tag = drop (length tag) $ take (length str - length tag + 1) str

addCodeBlock :: [String] -> PContent
addCodeBlock codeLines = PCodeBlockContent (PCodeBlock codeLines)

addList :: [PItem] -> PContent
addList items = PListContent (PList items)

