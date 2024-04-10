{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseJson
-}

module ParseJson.ParseJson (parseJson) where
import Content (PContent(..))

-- IO is for debug purpose but while be removed after to: parseMarkdown :: String -> Either String [PContent]
parseJson :: String -> IO (Either String [PContent]) -- return value to be define
parseJson file = return (Right [])
