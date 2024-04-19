--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala [WSL: Debian]
-- File description:
-- ParseFunction
--

module ParseJson.ParseFunction (notBracketChar, appendPContent, initPContent, lastPContent, appendCodeBlock) where
import Content (PContent(..), PParagraph(..), PParagraphType(..), PText(..), PTextType(..), PSection(..))
import Debug.Trace

notBracketChar :: String -> Bool
notBracketChar [] = False
notBracketChar (',':_) = True
notBracketChar (' ':_) = True
notBracketChar ('\n':_) = True
notBracketChar (':':_) = True
notBracketChar (_:_) = False

-- ajoute un nouveau PContent à la fin

retrieveTitle :: PContent -> String
retrieveTitle (PSectionContent (PSection {title = theTitle, section_content = _})) = theTitle
retrieveTitle _ = ""

checkLastContent :: PContent -> Bool
checkLastContent (PSectionContent (PSection {title = _, section_content = _})) = True
checkLastContent _ = False

addCodeBlock :: [String] -> Bool -> PContent -> [PContent] -> [PContent]
addCodeBlock [] _ newC (PSectionContent (PSection {title = _, section_content = content}):_) = trace (show newC ++ " : 1\n") content ++ [newC]
addCodeBlock [] _ newC content
    | checkLastContent (last content) = trace (show newC ++ " : 2\n") (init content) ++ [PSectionContent (PSection {title = retrieveTitle (last content), section_content = (addCodeBlock [] True newC [(last content)])})]
    | otherwise = trace (show newC ++ " : 2\n") content ++ [newC]
addCodeBlock ("section":xs) True newC (PSectionContent (PSection {title = theTitle, section_content = content}):_) = trace (show newC ++ " : 3\n") addCodeBlock (xs) True newC content
addCodeBlock ("section":xs) True newC content = trace (show newC ++ " : 4\n") (init content) ++ [PSectionContent (PSection {title = retrieveTitle (last content), section_content = (addCodeBlock (xs) True newC [(last content)])})]
addCodeBlock ("section":xs) False newC content = trace (show newC ++ " : 5\n") addCodeBlock (xs) True newC content
addCodeBlock (_:xs) isBody newC content = trace (show newC ++ " : 6\n") addCodeBlock (xs) isBody newC content
 
appendCodeBlock :: [String] -> PContent -> [PContent] -> [PContent]
appendCodeBlock state newC content = addCodeBlock state False newC content

addNewPContent :: [String] -> Bool -> PContent -> [PContent] -> [PContent]
addNewPContent [] _ newC (PSectionContent (PSection {title = _, section_content = content}):_) = trace (show newC ++ " : 1\n") content ++ [newC]
addNewPContent [] _ newC content = trace (show newC ++ " : 2\n") content ++ [newC]
addNewPContent ("section":xs) True newC content = trace (show newC ++ " : 4\n") (init content) ++ [PSectionContent (PSection {title = retrieveTitle (last content), section_content = (addNewPContent (xs) True newC [(last content)])})]
addNewPContent ("section":xs) False newC content = trace (show newC ++ " : 5\n") addNewPContent (xs) True newC content
addNewPContent (_:xs) isBody newC content = trace (show newC ++ " : 6\n") addNewPContent (xs) isBody newC content
 
appendPContent :: [String] -> PContent -> [PContent] -> [PContent]
appendPContent state newC content = addNewPContent state False newC content

-- supprime le dernier PContent

rmLastPContent :: [String] -> Bool -> [PContent] -> [PContent]
rmLastPContent [] _ (PSectionContent (PSection {title = theTitle, section_content = content}):_) = init content
rmLastPContent [] _ content = init content
rmLastPContent ("section":xs) True (PSectionContent (PSection {title = theTitle, section_content = content}):_) = rmLastPContent (xs) True content
rmLastPContent ("section":xs) True content = (init content) ++ [PSectionContent (PSection {title = retrieveTitle (last content), section_content = (rmLastPContent (xs) True [(last content)])})]
rmLastPContent ("section":xs) False content = rmLastPContent (xs) True content 
rmLastPContent (_:xs) isBody content = rmLastPContent (xs) isBody content

initPContent :: [String] -> [PContent] -> [PContent]
initPContent state content = rmLastPContent state False content

-- recupere le dernier PContent

getLastPContent :: [String] -> Bool -> [PContent] -> PContent
getLastPContent [] _ (PSectionContent (PSection {title = theTitle, section_content = content}):_) = last content
getLastPContent [] _ content = last content
getLastPContent ("section":xs) True (PSectionContent (PSection {title = theTitle, section_content = content}):_) = getLastPContent (xs) True content
getLastPContent ("section":xs) True content = getLastPContent (xs) True [(last content)]
getLastPContent ("section":xs) False content = getLastPContent (xs) True content
getLastPContent (_:xs) isBody content = getLastPContent (xs) isBody content

lastPContent :: [String] -> [PContent] -> PContent
lastPContent state content = getLastPContent state False content