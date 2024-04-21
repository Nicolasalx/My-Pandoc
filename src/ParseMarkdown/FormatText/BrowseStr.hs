--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- BrowseStr
--

module ParseMarkdown.FormatText.BrowseStr (browseStr) where
import ParseMarkdown.DataStructMarkdown (DataText(..))
import Content ()
import ParseMarkdown.FormatText.DetectSymbol (detectSymbol)

browseStr :: String -> DataText -> Bool -> DataText
browseStr [] dataText _ = dataText
browseStr (x:xs) dataText hasTakeFrstChar
    | not hasTakeFrstChar = 
        snd (detectSymbol ([x] ++ xs) (dataText { precedentChar = x })) `seq` 
        browseStr (fst (detectSymbol ([x] ++ xs) (dataText { precedentChar = x }))) 
                  (snd (detectSymbol ([x] ++ xs) (dataText { precedentChar = x }))) True
    | otherwise = 
        snd (detectSymbol xs (dataText { precedentChar = x })) `seq` 
        browseStr (fst (detectSymbol xs (dataText { precedentChar = x }))) 
                  (snd (detectSymbol xs (dataText { precedentChar = x }))) True
