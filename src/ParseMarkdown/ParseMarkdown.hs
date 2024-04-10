{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
-}

module ParseMarkdown.ParseMarkdown (parseMarkdown) where
import Content (PHeader(..), PBody(..))
-- import ParseMarkdown.ParseBody (parseBody)
import ParseMarkdown.ParseHeader (parseHeader)

-- THIS FUNCTION WILL CHANGE DON T WORRY !

parseMarkdown :: String -> IO (Either String (PHeader, PBody))
parseMarkdown file_content = do
    let allLines = lines file_content
    headerResult <- parseHeader allLines
    case headerResult of
        Right pHeader -> return $ Right (pHeader, PBody [])
        Left err -> return $ Left err
