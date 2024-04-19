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

browseStr :: String -> DataText -> Bool -> IO DataText
browseStr [] dataText _ = return dataText
browseStr (x:xs) dataText hasTakeFrstChar
    | not hasTakeFrstChar = do
        let newDataText = dataText { precedentChar = x }
        (newStr, finalDataText) <- detectSymbol ([x] ++ xs) newDataText
        browseStr newStr finalDataText True
    | otherwise = do
        let newDataText = dataText { precedentChar = x }
        (newStr, finalDataText) <- detectSymbol xs newDataText
        browseStr newStr finalDataText True
