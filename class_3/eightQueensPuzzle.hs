import Data.Char



chessboardsWith8Queens :: [[(Char, Int)]]
chessboardsWith8Queens =
    let chessboardsWithOneQueen = getChessboardsWithOneQueen
    in
        foldl
        (\chessboards stage
            -> filterChessboardsByPieces stage
                $ manageChessboardsWithAdditionalQueen chessboards)
        chessboardsWithOneQueen
        [2..6]


getChessboardsWithOneQueen :: [[(Char, Int)]]
getChessboardsWithOneQueen =
    [[(letter, number)] | letter <- ['a'..'a'], number <- [1..1]]


manageChessboardsWithAdditionalQueen :: [[(Char, Int)]] -> [[(Char, Int)]]
manageChessboardsWithAdditionalQueen chessboards =
    foldl
    (\allChessboards chessboard
        -> allChessboards ++ chessboardsWithAdditionalQueen chessboard)
    []
    chessboards



filterChessboardsByPieces :: Int -> [[(Char, Int)]] -> [[(Char, Int)]]
filterChessboardsByPieces piecesCount chessboards =
    filter
    (\chessboard
        -> piecesCount == length chessboard)
    chessboards


chessboardsWithAdditionalQueen :: [(Char, Int)] -> [[(Char, Int)]]
chessboardsWithAdditionalQueen chessboard =
    let possibleQueensPositions
            = getPossibleNewQueensPositions chessboard
    in  foldl
        (\chessboards newQueenPosition
            -> (newQueenPosition:chessboard):chessboards)
        []
        possibleQueensPositions



-- queensCollidesOnChessboard :: [(Char, Int)] -> Bool
-- queensCollidesOnChessboard queens =
--     fold (\(collide, remainderQueens)

getPossibleNewQueensPositions :: [(Char, Int)] -> [(Char, Int)]
getPossibleNewQueensPositions currentQueens =
    [(letter, number)
        |   letter <- ['a'..'h'],
            number <- [1..8],
            queenCollidesWithQueens (letter, number) currentQueens,
            fieldFree (letter, number) currentQueens]


fieldFree :: (Char, Int) -> [(Char, Int)] -> Bool
fieldFree = notElem
-- Dlaczego nie mogę :
-- fieldFree = not . elem


queenCollidesWithQueens :: (Char, Int) -> [(Char, Int)] -> Bool
queenCollidesWithQueens queen otherQueens =
    foldl
        (\collide position
            -> collide || queenCollidesWithQueen queen position)
        False
        otherQueens


queenCollidesWithQueen :: (Char, Int) -> (Char, Int) -> Bool
queenCollidesWithQueen (x1, y1) (x2, y2) =
    let distanceX = abs $ ord x1 - ord x2
        distanceY = abs $ y1 - y2
        isItself = x1 == x2 && y1 == y2
    in not isItself
        && (x1 == x2
                 || y1 == y2
                 || distanceX == distanceY)
