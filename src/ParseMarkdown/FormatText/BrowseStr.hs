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
    | not hasTakeFrstChar = do
        let (newStr, finalDataText) = detectSymbol ([x] ++ xs) (dataText { precedentChar = x })
        browseStr newStr finalDataText True
    | otherwise = do
        let (newStr, finalDataText) = detectSymbol xs (dataText { precedentChar = x })
        browseStr newStr finalDataText True
