import System.IO
import System.Environment
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Control.Monad

mainMain :: IO ()
mainMain = forever scrabbleMain

scrabbleMain :: IO ()
scrabbleMain = do
    word <- getLine
    let dictionaryPath = getDictionaryPath
    wordInDictionary <- checkIfWordInDictionary word dictionaryPath
    if wordInDictionary
        then manageExistingWord word
        else manageNotExistingWord

manageNotExistingWord :: IO ()
manageNotExistingWord =
    putStrLn "Word doesn't exists!"

manageExistingWord :: String -> IO ()
manageExistingWord word = do
    let wordPoints = getWordPoints word
        wordPointsAsString = wordPointsToString wordPoints
    putStrLn wordPointsAsString


wordPointsToString :: Maybe Int -> String
wordPointsToString Nothing = "0"
wordPointsToString (Just points) = show points


getWordPoints :: String -> Maybe Int
getWordPoints "" = Nothing
getWordPoints word = fmap sum $ sequence $ map getLetterValue word


getDictionaryPath :: String
getDictionaryPath = "class_6/dictionary.txt"


getLettersValues :: Map Char Int
getLettersValues = Map.fromList
    [
        ('A', 1), ('B', 3), ('C',3), ('D', 2), ('E', 1),
        ('F', 4), ('G', 2), ('H',4), ('I', 1), ('J', 8),
        ('K', 5), ('L', 1), ('M',3), ('N', 1), ('O', 1),
        ('P', 3), ('Q', 10), ('R',1), ('S', 1), ('T', 1),
        ('U', 1), ('V', 4), ('W',4), ('X', 8), ('Y', 4),
        ('Z', 10)
    ]


getLetterValue :: Char -> Maybe Int
getLetterValue letter = Map.lookup letterUpper letterValues
    where   letterValues = getLettersValues
            letterUpper = toUpper letter


checkIfWordInDictionary :: String -> String -> IO Bool
checkIfWordInDictionary word filePath = do
    fileContent <- readFile filePath
    let fileLines = lines fileContent
        wordExists = word `elem` fileLines
    return wordExists

-- Not used currently(14.05.2016)

getFileLinesCount :: String -> IO Int
getFileLinesCount filePath = do
    fileContent <- readFile filePath
    let fileLines = lines fileContent
        count = length fileLines
    return count


getLineFromFile :: Int -> String -> IO String
getLineFromFile lineNo filePath = do
    fileContent <- readFile filePath
    let fileLines = lines fileContent
        linesStartingFromNth = drop (lineNo - 1) fileLines
    case linesStartingFromNth of
        [] -> error "There is no such line in file!"
        l:_ -> return l
