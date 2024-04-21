--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala [WSL: Debian]
-- File description:
-- ParseFunction
--

module ParseJson.ParseFunction (notBracketChar, appendPContent, initPContent, lastPContent) where
import Content (PContent(..), PSection(..))

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

getLastPSection :: PContent -> [PContent]
getLastPSection (PSectionContent (PSection {title = _, section_content = contenu})) = contenu
getLastPSection _ = []

addNewPContent :: [String] -> Bool -> PContent -> [PContent] -> [PContent]
addNewPContent [] _ newC contenu = contenu ++ [newC]
addNewPContent ("section":xs) True newC contenu
    | checkLastContent (last contenu) = (init contenu) ++ [PSectionContent (PSection {title = retrieveTitle (last contenu), section_content = (addNewPContent xs True newC (getLastPSection (last contenu)))})]
addNewPContent ("section":xs) False newC contenu = addNewPContent (xs) True newC contenu
addNewPContent (_:xs) isBody newC contenu = addNewPContent (xs) isBody newC contenu
 
appendPContent :: [String] -> PContent -> [PContent] -> [PContent]
appendPContent state newC contenu = addNewPContent state False newC contenu

-- supprime le dernier PContent

rmLastPContent :: [String] -> Bool -> [PContent] -> [PContent]
rmLastPContent [] _ contenu = init contenu
rmLastPContent ("section":xs) True contenu
    | checkLastContent (last contenu) = (init contenu) ++ [PSectionContent (PSection {title = retrieveTitle (last contenu), section_content = (rmLastPContent xs True (getLastPSection (last contenu)))})]
rmLastPContent ("section":xs) False contenu = rmLastPContent (xs) True contenu 
rmLastPContent (_:xs) isBody contenu = rmLastPContent (xs) isBody contenu

initPContent :: [String] -> [PContent] -> [PContent]
initPContent state contenu = rmLastPContent state False contenu

-- recupere le dernier PContent

getLastPContent :: [String] -> Bool -> [PContent] -> PContent
getLastPContent [] _ contenu = last contenu
getLastPContent ("section":xs) True contenu = getLastPContent (xs) True (getLastPSection (last contenu))
getLastPContent ("section":xs) False contenu = getLastPContent (xs) True contenu
getLastPContent (_:xs) isBody contenu = getLastPContent (xs) isBody contenu

lastPContent :: [String] -> [PContent] -> PContent
lastPContent state contenu = getLastPContent state False contenu