module ParseXml.List (parseList) where

import Content (PBody(..), PContent(..), PParagraph(..), PParagraphType(..), PText(..), PBold(..), PItalic(..), PCode(..), PTextType(..), PSection(..), PCodeBlock(..), PList(..), PItem(..), PItemType(..), PImage(..), PLink(..)) 
import ParseXml.Paragraph (strip, removeParagraphEnd, stripTags)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)

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