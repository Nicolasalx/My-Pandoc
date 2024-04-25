module ParseXml.CodeBlock (parseCodeBlock) where

import Content (PBody(..), PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PImage(..), PLink(..)) 
import ParseXml.Paragraph (strip, removeParagraphEnd, stripTags)

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