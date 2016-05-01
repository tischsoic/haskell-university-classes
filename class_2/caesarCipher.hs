import Data.Char
import System.IO
import Control.Monad

caesarCipher :: Int -> String -> String
caesarCipher shift xs = map (caesarCipherOneChar shift) xs

caesarCipherOneChar :: Int -> Char -> Char
caesarCipherOneChar shift c =
    chr.(+32).( `mod` (126 - 32)).(+shift).(subtract 32).ord $ c
-- caesarCipherOneChar shift c = do
--   let charCode = ord c
--   let charCodeCountFromZero = charCode - 32
--   let charCodeOverflow = mod charCodeCountFromZero 126
--   let shiftedCharCode = charCodeOverflow + 32
--   let shiftedChar = chr shiftedChar
--   return shiftedChar


-- 32 - 126

unCaesarCipher :: Int -> String -> String
unCaesarCipher shift xs = map (caesarCipherOneChar (-shift)) xs

getFileContent = do
    contents <- readFile "class_2/file_1.txt"
    let wlist = lines contents
    return wlist


-- getFileCharStatistics :: FilePath -> IO [(Char, Int)]
getFileCharStatistics filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    let emptyStatistics = getEmptyStatistics
        asd = foldM
            (\acc line
                -> addStringStatisticSafe line [acc])
            emptyStatistics
            fileLines
        count = statisticsDeflection getEmptyStatistics asd
    hClose handle
    return ()

addStringStatisticSafe :: String -> [(Char, Int)] -> [(Char, Int)]
addStringStatisticSafe line statistics
    = foldl
        (flip addLetterStatisticSafe)
        statistics
        line

addLetterStatisticSafe :: Char -> [(Char, Int)] -> [(Char, Int)]
addLetterStatisticSafe letter statistics
    | countInLetterToStatistics letterLowercase
        = addLetterToStatistics letterLowercase statistics
    | otherwise = statistics
    where   letterLowercase = toLower letter


addLetterStatistic :: Char -> [(Char, Int)] -> [(Char, Int)]
addLetterStatistic letter statistics
    =   map (\statistic@(letterFromList, occurencesNo) ->
            if letterFromList == letter
            then (letterFromList, occurencesNo + 1)
            else statistic)
        statistics


lettersCountByStatistics :: [(Char, Int)] -> Int
lettersCountByStatistics statistics =
    foldl (\acc (_, count) -> acc + count) 0 statistics


countInLetterToStatistics :: Char -> Bool
countInLetterToStatistics x =
    let lowerBoundary = 97
        upperBoundary = 122
        letterNumber = ord x
    in letterNumber >= lowerBoundary
        && letterNumber <= upperBoundary


divideInts :: Int -> Int -> Float
divideInts a b = fromIntegral a / fromIntegral b


statisticsDeflection :: [(Char, Int)] -> [(Char, Int)] -> Float
statisticsDeflection statistics baseStatistics =
    let lettersCount = lettersCountByStatistics statistics
        baseLettersCount = lettersCountByStatistics baseStatistics
        proportion = divideInts lettersCount baseLettersCount
        deflection = foldl
            (\acc ((_, count), (_, baseCount))
                -> acc + computeDeflation count baseCount proportion)
            0.0 $ zip statistics baseStatistics
        proportionalDeflection = deflection / fromIntegral lettersCount
    in proportionalDeflection


computeDeflation :: Int -> Int -> Float -> Float
computeDeflation count baseCount proportion
    = abs (fromIntegral baseCount * proportion - fromIntegral count)


getEmptyStatistics :: [(Char, Int)]
getEmptyStatistics = zip ['a'..'z'] [0,0..]


getEnglishLangBaseStatistics :: [(Char, Int)]
getEnglishLangBaseStatistics =
    zip ['a'..'z'] [1..2]


--- UNUSED ------------------------------

addLetterToStatistics :: Char -> [(Char, Int)] -> [(Char, Int)]
addLetterToStatistics letter statistics
    | statisticForLetterExtists letter statistics
        =   map (\statistic@(letterFromList, occurencesNo) ->
                if letterFromList == letter
                then (letterFromList, occurencesNo + 1)
                else statistic)
            statistics
    | otherwise = (letter, 1):statistics


statisticForLetterExtists :: Char -> [(Char, Int)] -> Bool
statisticForLetterExtists letter statistics =
    foldl (\acc (l, _) -> acc || l == letter) False statistics
