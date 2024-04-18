--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseMarkdown
--

module ParseMarkdown.ParseMarkdown (parseMarkdown) where
import Content (PHeader(..), PBody(..))
import ParseMarkdown.ParseBody (parseBody)
import ParseMarkdown.ParseHeader (parseHeader)
import ParseMarkdown.DataStructMarkdown (initializeDataParsing)

-- THIS FUNCTION WILL CHANGE DON T WORRY !

parseMarkdown :: String -> IO (Either String (PHeader, PBody))
parseMarkdown file_content = do
    let dataInitialized = initializeDataParsing
    let allLines = lines file_content
    (headerResult, newDataParsing) <- parseHeader allLines dataInitialized
    pBodyResult <- parseBody newDataParsing
    case pBodyResult of
        Right pBody -> do
            -- print pBody
            case headerResult of
                Right pHeader -> return $ Right (pHeader, pBody)
                Left err -> return $ Left err
        Left err -> do
            -- print err
            return $ Left err

