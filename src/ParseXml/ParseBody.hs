module ParseXml.ParseBody (parseBody) where

import Content (PBody(..), PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PImage(..), PLink(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Debug.Trace
import ParseXml.Sections (parseSections)

parseBody :: String -> IO (Either String PBody)
parseBody file_content = do
    let linesContent = lines file_content
        sections = parseSections linesContent
    return $ Right (PBody sections)
