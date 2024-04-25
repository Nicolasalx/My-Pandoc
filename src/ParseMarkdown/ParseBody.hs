{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- ParseBody
-}

module ParseMarkdown.ParseBody (parseBody) where
import Content (PBody(..))
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import ParseMarkdown.FormatText.CreateText (createText)
import ParseMarkdown.ParseElem.ParseAllStrings (parseAllString)
import ParseMarkdown.ParseElem.Paragraph (tryAddParagraph)

parseBody :: DataParsing -> Either String PBody
parseBody dataParsing
    | Left err <- parseResult,
      _ <- newDataParsed = Left err
    | Right contents <- parseResult,
      dataPars <- createText newDataParsed,
      (newContent, _) <- tryAddParagraph dataPars contents =
        Right (PBody newContent)
    where
        (parseResult, newDataParsed) =
            parseAllString (remainingLines dataParsing) dataParsing []
