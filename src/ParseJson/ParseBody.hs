--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
--

module ParseJson.ParseBody (parseBody) where
import Content (PBody(..))

parseBody :: String -> IO (Either String PBody)
parseBody file_content = return $ Right (PBody [])
