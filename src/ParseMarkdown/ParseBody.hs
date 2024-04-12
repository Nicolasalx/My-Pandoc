--
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
-- File description:
-- parseBody
--

module ParseMarkdown.ParseBody (parseBody) where
import Content (PContent(..), PText(..))
import ParseMarkdown.DataStructMarkdown (DataParsing(..))
import ParseMarkdown.ParseOneChar (parseOneChar)

parseBody :: DataParsing -> IO (Either String [PContent])
parseBody dataParsing = do
    let allContent = []
    (allContent, dataParsed) <- parseAllString (remainingLines dataParsing) dataParsing allContent
    print allContent
    return (Left "ok")

parseAllString :: [String] -> DataParsing -> [PContent] -> IO (Either String [PContent], DataParsing)
parseAllString [] dataParsing allContent = return (Right allContent, dataParsing)
parseAllString (x:xs) dataParsing allContent = do
    stringParsed <- parseEachString x dataParsing
    let (newContent, newDataParsed) = tryAddElemToContent stringParsed allContent
    dataParsed <- return newDataParsed { nbReturnLines = nbReturnLines dataParsing + 1 }
    parseAllString xs dataParsed newContent

parseEachString :: String -> DataParsing -> IO DataParsing
parseEachString [] dataParsing = return dataParsing
parseEachString (c:cs) dataParsing = do
    dataParsed <- parseOneChar c dataParsing
    parseEachString cs dataParsed

tryAddElemToContent :: DataParsing -> [PContent] -> ([PContent], DataParsing)
tryAddElemToContent dataParsing allContent
    | nbReturnLines dataParsing > 1 = addNewParagraph dataParsing allContent
    | otherwise = (allContent, dataParsing)

------------------------------------------------------------------------------------------------------------
-----------------------------------            PARAGRAPH               -------------------------------------
------------------------------------------------------------------------------------------------------------

addNewParagraph :: DataParsing -> [PContent] -> ([PContent], DataParsing)
addNewParagraph dataParsing allContent = do
    let actualContent = initializePTextContent (actualList dataParsing)
    let parsedData = dataParsing {actualList = ""}
    (addNewElemToContent actualContent allContent, parsedData)

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

addNewElemToContent :: PContent -> [PContent] -> [PContent]
addNewElemToContent actualContent allContent = allContent ++ [actualContent]

initializePText :: String -> PText
initializePText str = PText [Right str]

initializePTextContent :: String -> PContent
initializePTextContent str = PTextContent (initializePText str)










-- Si on a une string et que ce n'est pas:
-- Header OU Link ALT OU IMAGE ALT
-- Alors c'est contenu dans un paragraphe

-- Si ca ne débute pas par un header

-- Tous les noms de section ne peuvent pas etre modifier avec le formattage d'un texte (C'est juste une string)
























{-

Différents éléments qui montrent comment est formatter un élément:

Text

Si un seul retour à la ligne c'est un paragraphe qui continue
Si deux retour alors ca devient un paragraphe



Links And Images:

1) Links :
- [
- Text
- ]
- (
- String
- )

2) Images :
- ![
- String


-}

-- Se balader dans chaque lines de remainingLines
    -- Analyser la ligne actuel
        -- Si
    -- Puis vérifier le status actuel
