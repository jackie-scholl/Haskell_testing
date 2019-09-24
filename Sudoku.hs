module Main where
    
import qualified Data.List as L
import qualified Data.Map as  M
import qualified Data.Set as  S

import Debug.Trace (trace)

-- (row, column)

-- custom number type to avoid out of bounds (e.g., 0 or 10) or improper operations
-- (e.g. adding two numbers) doesn't make sense in Sudoku
data Num9 = Z1 | Z2 | Z3 | Z4 | Z5 | Z6 | Z7 | Z8 | Z9 deriving (Eq, Ord, Show, Enum)

nums :: [Num9]
nums = [Z1 ..]

intToNum9 :: Int -> Num9
intToNum9 x = nums !! (x - 1)

num9ToInt :: Num9 -> Int 
num9ToInt n = maybe 0 (1+) (L.elemIndex n nums)

type NumSet = S.Set Num9

fullSet :: NumSet
fullSet = S.fromList nums

type Location = (Num9, Num9)

-- a Sudoku puzzle is a map from locations on the grid to a list of numbers that
-- could be in that grid square
type Sudoku = M.Map Location NumSet

-- cross multiply two lists
cross :: [a] -> [b] -> [(a, b)]
cross xs ys = [(x, y) | x <- xs, y <- ys]

allLocs :: [Location]
allLocs = cross nums nums

column :: Num9 -> [Location]
column n = [(x, n) | x <- nums]

-- given a location on the board, get all locations in the same column
reverseColumn :: Location -> [Location]
reverseColumn (_, y) = column y

row :: Num9 -> [Location]
row n = [(n, x) | x <- nums]

-- given a location on the board, get all locations in the same row
reverseRow :: Location -> [Location]
reverseRow (x, _) = row x

-- a box is a 3x3
-- example: box Z1 = 
-- [(Z1,Z1),(Z1,Z2),(Z1,Z3),(Z2,Z1),(Z2,Z2),(Z2,Z3),(Z3,Z1),(Z3,Z2),(Z3,Z3)]
box :: Num9 -> [Location]
box n =
    let a = [Z1 .. Z3]
        b = [Z4 .. Z6]
        c = [Z7 .. Z9]
        xs = [(Z1, (a, a)), (Z2, (a, b)), (Z3, (a, c)),
             (Z4, (b, a)), (Z5, (b, b)), (Z6, (b, c)),
             (Z7, (c, a)), (Z8, (c, b)), (Z9, (c, c))]
    in cross2 (snd (head (filter (\x -> fst x == n) xs)))
    where
        cross2 :: ([a], [a]) -> [(a, a)]
        cross2 (xs, ys) = cross xs ys

-- like reverseRow and reverseColumn but for boxes
-- terrible algorithm, fix later
reverseBox :: Location -> [Location]
reverseBox l = head $ filter (L.elem l) $ map box nums

-- all locations that are in the same row, column, or box as the given location
neighbors :: Location -> S.Set Location
neighbors loc =
    S.delete loc $ S.fromList $ concat
    $ map (\f -> f loc) [reverseRow, reverseColumn, reverseBox]


converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- Updates a specific square and performs the necessary changes that result
modifyAndPropogate :: (NumSet -> NumSet) -> Location -> Sudoku -> Sudoku
modifyAndPropogate f l s =
    let
        current = s M.! l 
        changed = f current
        s' = M.insert l changed s
    in case compare (S.size changed) 1 of
        LT -> error "bad puzzle"
        GT -> s'
        EQ -> if (S.size current /= 1)
                then (propogateDelete (S.findMax changed) l s')
                else (s')
    where
        propogateDelete :: Num9 -> Location -> Sudoku -> Sudoku 
        propogateDelete n l2 s2 =
            foldr (modifyAndPropogate (S.delete n)) s2 (neighbors l2)

setAndPropogate :: Num9 -> Location -> Sudoku -> Sudoku 
setAndPropogate n l s = modifyAndPropogate (const $ S.singleton n) l s

-- All transform functions are algorithms that try to narrow a puzzle's possible states
-- to be quite honest, I no longer remember how most of them work
-- I should have commented better when I wrote this
transform2 :: [Location] -> Sudoku -> Sudoku 
transform2 ls s = 
    foldr (uncurry setAndPropogate) s simplified
    where
        combined = concat $ map (S.toList . (s M.!)) ls
        combinedMap = toOccurrenceMap combined
        singles = M.keys $ M.filter (1 ==) combinedMap
        listPairs = map (\x -> (x, filter ((S.member x) . (s M.!)) ls)) singles
        listPairs' = filter (\x -> (L.length $ snd x) == 1) listPairs
        simplified = map (\x -> (fst x, head $ snd x)) listPairs'

        numTimesFound x xs = (length . filter (== x)) xs
        toOccurrenceMap :: Ord a => [a] -> M.Map a Int
        toOccurrenceMap xs = M.fromList $ map (\x -> (x, numTimesFound x xs)) $ L.nub xs

transform2' :: Sudoku -> Sudoku
transform2' s = foldr transform2 s allGroups

transform2'' :: Sudoku -> Sudoku
transform2'' = converge ((flip (foldr transform2) allGroups) . id)


transform3 :: [Location] -> Sudoku -> Sudoku 
transform3 ls s = 
    let 
        ls' :: [Location]
        ls' = filter (\x -> (S.size $ s M.! x) > 1) ls
        pairs = [(s M.! x, s M.! y) | x <- ls', y <- ls', x > y]
        filteredPairs = filter ((2 ==) . S.size . uncurry S.union) pairs
        s' = s
        s'' = foldl (f ls') s' filteredPairs
    in s''
    where
        f :: [Location] -> Sudoku -> (NumSet, NumSet) -> Sudoku
        f ls' s2 (a, _) = foldr (modifyAndPropogate (f2 a)) s2 ls'
        
        f2 :: NumSet -> NumSet -> NumSet
        f2 x y | x == y     = x
               | otherwise  = S.difference y x


transform4 :: Int -> [Location] -> Sudoku -> Sudoku 
transform4 n ls s = 
   let ls' :: [Location]
       ls' = filter (\x -> (S.size $ s M.! x) > 1) ls
       groups = map (map (s M.!)) $ locationSubsets n ls'
       filteredGroups = filter ((n ==) . S.size . S.unions) groups
       s'' = foldl (f ls') s filteredGroups
   in s''
   where
       f :: [Location] -> Sudoku -> [NumSet] -> Sudoku
       f ls' s2 group = foldr (modifyAndPropogate $ f2 $ S.unions group) s2 ls'
       
       f2 :: NumSet -> NumSet -> NumSet
       f2 x y | y `S.isSubsetOf` x     = y
              | otherwise              = S.difference y x
              
       locationSubsets :: Int -> [Location] -> [[Location]]
       locationSubsets n' pool = locationSubsets' n' pool []

       locationSubsets' :: Int -> [Location] -> [Location] -> [[Location]]
       locationSubsets' 0    _ soFar = [soFar]
       locationSubsets' n' pool soFar = concat $ map (\x -> locationSubsets' (n'-1) (filter (x<) pool) (x:soFar)) pool

allGroups :: [[Location]]
allGroups = map (\x -> (fst x) (snd x)) $ cross [column, row, box] nums

transform3'' :: Sudoku -> Sudoku
transform3'' = converge (sub . transform2'')
    where sub s = (flip (foldr transform3) allGroups) s

transform4'' :: Int -> Sudoku -> Sudoku 
transform4'' 1 s = transform2'' s
transform4'' n s = converge ((\x -> foldr (transform4 n) x allGroups) . (transform4'' $ n-1)) s

transformIntersection :: [Location] -> [Location] -> Sudoku -> Sudoku 
transformIntersection a b s =
    let fullI = S.intersection (S.fromList a) (S.fromList b)
        a' = S.filter (\x -> (S.size $ s M.! x) > 1) $ S.fromList a
        b' = S.filter (\x -> (S.size $ s M.! x) > 1) $ S.fromList b
        i = S.intersection a' b'
        ax = S.difference a' i 
        bx = S.difference b' i
        i2 = S.unions $ map (s M.!) $ S.toList i 
        ax2 = S.unions $ map (s M.!) $ S.toList ax
        i2' = S.toList $ S.difference i2 ax2
        --s' = 
    in if (S.size fullI == 3 && S.size i > 0 && L.length i2' > 0)
        then (foldr (f $ S.toList bx) s i2')
        else (s)
    where
        f x n s = foldr (modifyAndPropogate (S.delete n)) s x

--transformIntersection' s = foldr (uncurry transformIntersection) s $ cross allGroups allGroups

transformIntersection'' :: Sudoku -> Sudoku
transformIntersection'' = converge (sub . (transform4'' 6))
    where
        sub s = foldr (uncurry transformIntersection) s $ cross allGroups allGroups

checkSolution :: Sudoku -> Bool
checkSolution s = all checkGroupFunc [column, row, box]
    where
        checkGroupFunc :: (Num9 -> [Location]) -> Bool
        checkGroupFunc f = all checkGroupLocs (map f nums)

        checkGroupLocs :: [Location] -> Bool
        checkGroupLocs ls = checkGroupSets $ map (s M.!) ls
        
        checkGroupSets :: [NumSet] -> Bool
        checkGroupSets xs = nums == (L.sort $ concat $ map S.toList xs)

checkCurrent :: Sudoku -> Bool
checkCurrent s = all checkGroupFunc [column, row, box]
    where
        checkGroupFunc :: (Num9 -> [Location]) -> Bool
        checkGroupFunc f = all checkGroupLocs (map f nums)

        checkGroupLocs :: [Location] -> Bool
        checkGroupLocs ls = checkGroupSets $ map (s M.!) ls
        
        checkGroupSets :: [NumSet] -> Bool
        checkGroupSets xs = 
            let individuals = map S.findMax $ filter (\ns -> S.size ns == 1) xs
            in (all (\ns -> S.size ns > 0) xs) && (L.length individuals) == (S.size $ S.fromList individuals)


printSudoku :: Sudoku -> String
printSudoku s =
    let
        blankLine = "\n______|_______|______\n"
    in printTriple [Z1 .. Z3] ++ blankLine
        ++ printTriple [Z4 .. Z6] ++ blankLine
        ++ printTriple [Z7 .. Z9] ++ "\n"
        ++ "Total possibilities: " ++ show possibilities ++ "\n"
        ++ "Solution? " ++ show (checkSolution s) ++ "\n"
        ++ "Currently valid? " ++ show (checkCurrent s) ++ "\n"
    where
        printTriple :: [Num9] -> String
        printTriple rows = foldl (++) "" $ L.intersperse "\n" (helper rows)
        
        helper :: [Num9] -> [String]
        helper rows = map (\r -> printRow s r) rows
        
        possibilities = M.foldr (+) 0 $ M.map S.size s
    
printRow :: Sudoku -> Num9 -> String
printRow s n =
    printTriple [Z1 .. Z3] ++ " | " ++ printTriple [Z4 .. Z6] ++ " | " ++ printTriple [Z7 .. Z9]
    where
        printTriple :: [Num9] -> String
        printTriple cols = foldl (++) "" $ L.intersperse " " (helper cols)
        
        helper :: [Num9] -> [String]
        helper cols = map (\c -> printSquare s (n, c)) cols

printSquare :: Sudoku -> Location -> String
printSquare s l = 
    let res = M.lookup l s
    in mapRes res
    where
        mapRes :: Maybe NumSet -> String
        mapRes (Just x) = printNumSet x
        mapRes Nothing = "E"

printNumSet :: NumSet -> String
printNumSet ns | S.size ns == 0     = "X"
               | S.size ns == 1     = show $ num9ToInt $ S.findMax ns
               | True               = " "

blank :: Sudoku
blank = M.fromList [((x, y), fullSet) | x <- nums, y <- nums]

testGrid :: [(Int, Int, Int)]
testGrid = [(1, 2, 4), (1, 3, 5), (1, 5, 1), (1, 7, 7),
            (2, 1, 6), (2, 2, 1), (2, 5, 5), (2, 6, 9), (2, 8, 8),
            (3, 6, 3), (3, 7, 2), (3, 8, 1),
            (4, 1, 5), (4, 2, 2), (4, 9, 9),
            (5, 3, 4), (5, 7, 6),
            (6, 1, 9), (6, 8, 5), (6, 9, 3),
            (7, 2, 9), (7, 3, 6), (7, 4, 5),
            (8, 2, 5), (8, 4, 2), (8, 5, 3), (8, 8, 4), (8, 9, 7),
            (9, 3, 7), (9, 5, 4), (9, 7, 5), (9, 8, 6)]
            
{-testGridB :: [(Int, Int, Int)]
testGridB = [(1, 1, 5), (1, 2, 2), (1, 4, 1), (1, 8, 6),
             (2, 3, 9), (2, 6, 5), (2, 7, 2), (2, 9, 3),
             (3, 8, 4),
             (4, 1, 3), (4, 4, 5), (4, 9, 2),
             (5, 5, 7),
             (6, 1, 2), (6, 6, 8), (6, 9, 4),
             (7, 2, 4),
             (8, 1, 1), (8, 3, 7), (8, 4, 3), (8, 7, 4),
             (9, 2, 9), (9, 6, 6), (9, 8, 3), (9, 9, 1)]-}

{-testGridB :: [(Int, Int, Int)]
testGridB = [(1, 1, 6), (1, 4, 9), (1, 6, 2),
             (2, 3, 7), (2, 7, 4),
             (3, 1, 1), (3, 9, 6),
             (4, 1, 4), (4, 3, 3), (4, 5, 1), (4, 6, 7), (4, 8, 5),
             (5, 2, 1), (5, 5, 9), (5, 8, 8),
             (6, 2, 6), (6, 4, 3), (6, 5, 5), (6, 7, 1), (6, 9, 4),
             (7, 1, 3), (7, 9, 9),
             (8, 3, 6), (8, 7, 3),
             (9, 4, 7), (9, 6, 9), (9, 9, 5)
            ]-}
            
testC = "10400000000020000000000050407008000300001090000300400200050100000000806000"
testGridB :: [(Int, Int, Int)]
testGridB = concat $ map (\x -> map (\y -> ((y `div` 9) + 1, (y `mod` 9) + 1, fst x)) $ snd x) $ map (\x -> (x, L.elemIndices (head $ show x) testC)) ([1..9] :: [Int])

transformSimpleInput :: (Int, Int, Int) -> (Location, Num9)
transformSimpleInput (x, y, c) = ((intToNum9 x, intToNum9 y), intToNum9 c)

gridToSudoku :: [(Int, Int, Int)] -> Sudoku
gridToSudoku g = foldl (flip $ uncurry $ flip setAndPropogate) blank (map transformSimpleInput g)

gridToSudoku2 :: [(Int, Int, Int)] -> Sudoku
gridToSudoku2 g = M.union (M.fromList $ map (\x -> (fst x, S.singleton $ snd x)) $ map transformSimpleInput g) blank

testS :: Sudoku
testS = gridToSudoku testGrid

testS7 = transform2'' testS

testBs :: [Sudoku]
testBs = 
    let testB = gridToSudoku testGridB
    in [
    gridToSudoku2 testGridB,
    testB,
    transform2'' testB,
    transformIntersection'' testB
    ]

main :: IO ()
main = do
    
    putStrLn $ concat $ L.intersperse "\n---1---\n" $ map printSudoku testBs
    

    return ()