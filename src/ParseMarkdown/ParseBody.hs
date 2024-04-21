--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseBody
--

module ParseMarkdown.ParseBody (parseBody) where
import Content (PBody(..))
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import ParseMarkdown.FormatText.CreateText (createText)
import ParseMarkdown.ParseElem.ParseAllStrings (parseAllString)
import ParseMarkdown.ParseElem.Paragraph (tryAddParagraph)

parseBody :: DataParsing -> Either String PBody
parseBody dataParsing =
    let (allContent, newDataParsed) = parseAllString (remainingLines dataParsing) dataParsing []
    in case allContent of
        Left err -> Left err
        Right contents ->
            let dataPars = createText newDataParsed
                (newContent, _) = tryAddParagraph dataPars contents
            in Right (PBody newContent)

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

-- TODO LIST

-- ! When a codeBlock is Open but don't close -> The program wait the codeBlock close => Find a solution to this
    -- ! Solution : Make a function to check if the codeblock is closed

-- ! If a Section is in codeBlock the result is this: [PSectionContent (PSection {title = "Section A", section_content = [PCodeBlockContent (PCodeBlock ["","","abc",""])]})]
-- ```
-- # Section A
-- 
-- abc
-- 
-- ```

-- ! If a [ is Open but not close (if it's not a link i will try to get all the line and not continue)
    -- ! Solution : Make a function to check if the link is closed 

-- ! Add a section with ====