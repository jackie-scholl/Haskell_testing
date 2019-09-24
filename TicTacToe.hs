import Data.Maybe
import Data.List
import Debug.Trace

data Player = X | O deriving (Eq, Show)
type Board = [[Maybe Player]]
type Coord = (Int, Int)

moveList :: [Coord]
moveList = [(0, 0), (0, 1), (1, 0), (0, 2), (1, 1), (1, 2), (2, 0)]

playGame :: [Coord] -> [Board]
playGame coords = scanl move emptyBoard coords

final :: Board
final = last $ playGame moveList

main :: IO ()
main = do
    putStrLn $ concat $ map printBoard $ playGame moveList
    --putStrLn $ show $ chooseMove $ last $ playGame moveList

printBoard :: Board -> String
printBoard b = printGameState b ++ printBoard' b ++ "\n\n"
    where
        printSpace :: Maybe Player -> String
        printSpace (Just p) = show p
        printSpace (Nothing) = "_"
        
        printRow :: [Maybe Player] -> String
        printRow spaces = concat $ map printSpace spaces
        
        printBoard' :: [[Maybe Player]] -> String
        printBoard' rows = intercalate "\n" $ map printRow rows
        
        printGameState :: Board -> String
        printGameState board
            | not $ isGameOver board = "Game is not yet over; current turn: " ++ show (getCurrentTurn board) ++ "\n"
            | otherwise = "Game over, winner is " ++ show (checkWinner board) ++ "\n"
        
emptyBoard :: Board
emptyBoard = [emptyLine, emptyLine, emptyLine]
    where emptyLine = [Nothing, Nothing, Nothing]

lengths :: Board -> [Int]
lengths = map length

isBoardRightSize :: Board -> Bool
isBoardRightSize board = (length board == 3)
    && and (map (== 3) $ map length board)

countEmpties :: Board -> Int
countEmpties board = sum $ map helper board
    where
        helper :: [Maybe Player] -> Int
        helper x = length $ filter isNothing x

allLines :: [[Coord]]
allLines = [[(x, y) | x <- [0..2]] | y <- [0..2]] ++
           [[(y, x) | x <- [0..2]] | y <- [0..2]] ++
           [[(0,0),(1,1),(2,2)], [(2,0),(1,1),(0,2)]]

-- ugly but works
testLine :: [Maybe Player] -> Maybe Player
testLine line = if (all (== Just X) line) then (Just X) else
                if (all (== Just O) line) then (Just O) else
                (Nothing)

getCoord :: Board -> Coord -> Maybe Player
getCoord b (x, y) = (b !! x) !! y

extractLines :: Board -> [[Maybe Player]]
extractLines board = map (map (getCoord board)) allLines

checkWinner :: Board -> Maybe Player
checkWinner board = foldl helper Nothing $ map testLine (extractLines board)
    where
        helper Nothing (Just x) = Just x
        helper Nothing Nothing = Nothing
        helper (Just x) _ = Just x

isGameOver :: Board -> Bool
isGameOver board = (isJust $ checkWinner board) || all (\c -> all isJust c) board

getCurrentTurn :: Board -> Player
getCurrentTurn b = if ((countEmpties b) `mod` 2 == 0) then O else X

modifyAtIndex :: [a] -> Int -> (a -> a) -> [a]
modifyAtIndex list index modifier 
    | (index >= length list) || (index < 0) = list -- index out of bounds; could throw error instead?
    | otherwise = (take index list) ++ [modifier $ list !! index] ++ (drop (index + 1) list)

move :: Board -> Coord -> Board
move board coord@(x, y) 
    | isJust $ getCoord board coord = undefined
    | otherwise = modifyAtIndex board x rowModifier --replaceAtIndex x () (board)
    where
        rowModifier row = modifyAtIndex row y $ const $ Just $ getCurrentTurn board

score :: Board -> Player -> Int
score b p 
    | isGameOver b = maybe 0 (playerMapper p) (checkWinner b)
    | otherwise = otherScore --maybe (otherScore) (playerMapper p) (checkWinner b)
    where
        playerMapper :: Player -> Player -> Int
        playerMapper p1 p2 = if (p1 == p2) then 1 else (-1)
        moveChoice :: Coord
        moveChoice = chooseMove b
        otherScore :: Int
        otherScore = score (move b moveChoice) p


chooseMove :: Board -> Coord
chooseMove b
    | isGameOver b = undefined
    | otherwise = head {-$ trace ("empties are " ++ show empties)-} $ sortOn (\c -> score (move b c) (getCurrentTurn b)) empties
    where
        allCoords = [(x, y) | x <- [0..2], y <- [0..2]]
        empties = filter (\c -> getCoord b c == Nothing) allCoords

-- Haskell didn't allow me to try the complex, difficult work that Idris allowed.
-- I had an error where I had [Bool] but needed Boo
-- I'm a little scared of messing up, this program feels hard to read
-- It feels fuzzy in some sense
-- I confused checkWinner() with a game over check at first, didn't notice until
-- AI started throwing errors which were difficult to debug
