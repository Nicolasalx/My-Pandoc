{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseXml
-}

module ParseXml.ParseXml (parseXml) where
import Content (PContent(..))

-- IO is for debug purpose but while be removed after to: parseMarkdown :: String -> Either String [PContent]
parseXml :: String -> IO (Either String [PContent])
parseXml file = return (Right [])
