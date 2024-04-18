--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala [WSL: Debian]
-- File description:
-- ParseFunction
--

module ParseJson.ParseFunction (notBracketChar, appendPContent, initPContent, lastPContent) where
import Content (PContent(..), PParagraph(..), PParagraphType(..), PText(..), PTextType(..), PSection(..))

notBracketChar :: String -> Bool
notBracketChar [] = False
notBracketChar (',':_) = True
notBracketChar (' ':_) = True
notBracketChar ('\n':_) = True
notBracketChar (':':_) = True
notBracketChar (_:_) = False

-- ajoute un nouveau PContent à la fin

retrieveTitle :: PContent -> String
retrieveTitle (PSectionContent (PSection {title = title, section_content = _})) = title
retrieveTitle _ = ""

addNewPContent :: [String] -> Bool -> PContent -> [PContent] -> [PContent]
addNewPContent [] _ newC content = content ++ [newC]
addNewPContent ("section":xs) True newC content = (init content) ++ [PSectionContent (PSection {title = retrieveTitle (last content), section_content = (addNewPContent (xs) True newC [(last content)])})]
addNewPContent ("section":xs) False newC content = addNewPContent (xs) True newC content
addNewPContent (_:xs) isBody newC content = addNewPContent (xs) isBody newC content
 
appendPContent :: [String] -> PContent -> [PContent] -> [PContent]
appendPContent state newC content = addNewPContent state False newC content

-- supprime le dernier PContent

rmLastPContent :: [String] -> Bool -> [PContent] -> [PContent]
rmLastPContent [] _ content = init content
rmLastPContent ("section":xs) True content = (init content) ++ [PSectionContent (PSection {title = retrieveTitle (last content), section_content = (rmLastPContent (xs) True [(last content)])})]
rmLastPContent ("section":xs) False content = rmLastPContent (xs) True content 
rmLastPContent (_:xs) isBody content = rmLastPContent (xs) isBody content

initPContent :: [String] -> [PContent] -> [PContent]
initPContent state content = rmLastPContent state False content

-- recupere le dernier PContent

getLastPContent :: [String] -> Bool -> [PContent] -> PContent
getLastPContent [] _ content = last content
getLastPContent ("section":xs) True content = getLastPContent (xs) True [(last content)]
getLastPContent ("section":xs) False content = getLastPContent (xs) True content
getLastPContent (_:xs) isBody content = getLastPContent (xs) isBody content

lastPContent :: [String] -> [PContent] -> PContent
lastPContent state content = getLastPContent state False content