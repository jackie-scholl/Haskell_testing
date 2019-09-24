# Chapter 9: Lists

## 9.5 Using ranges to construct lists

### Exercise: EnumFromTo

I cannot figure out what this is asking 🤷‍♀️

## 9.6 Extracting portions of lists

### Exercises: Thy Fearful Symmetry

1.
splitWords :: `[Char]` -> `[[Char]]`
splitWords "" = []
splitWords xs = first : splitWords rest
    where first = takeWhile cond xs
          rest = drop 1 $ dropWhile cond xs
          cond = (/= ' ')
          
2.
splitLines :: `[Char]` -> `[[Char]]`
splitLines "" = []
splitLines xs = first : splitLines rest
    where first = takeWhile cond xs
          rest = drop 1 $ dropWhile cond xs
          cond = (/= ' ')
          
3.

splitString :: `[Char]` -> `[[Char]]`
splitString "" _ = []
splitString xs sep = first : splitString rest sep
    where first = takeWhile cond xs
          rest = drop 1 $ dropWhile cond xs
          cond = (/= sep)

myWords = flip splitString ' '
myLines = flip splitString '\n'

## 9.7 List comprehensions

### Exercises: Comprehend Thy Lists

1. 4, 16, 36, 49, 64, 100
2. [(1,64), (1, 81), (1, 100), (4, 64), ..., (49, 64), (49, 81), (49, 100)]
3. [(1,64), (1, 81), (1, 100), (4, 64), (4, 81)]

### Exercises: Square Cube

1. [(x, y) | x <- mySqr, y <- myCube]
2. [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
3. length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]


## 9.8 Spines and nonstrict evaluation

### Exercises: Bottom Madness
#### Will it blow up?

1. Yes
2. No
3. Yes
4. No
5. Yes
6. No
7. Yes
8. No
9. No
10. Yes

### Intermission: Is it in normal form?

1. NF
2. WHNF
3. Neither
4. Neither
5. Neither
6. Neither
7. NF

## 9.9 Transforming lists of values

### Exercises: More Bottoms

1. No
2. Yes
3. No
(tho I suppose they all return *a value* and you can examine the spine of the list just fine...)

4. returns a list saying for each character in the string whether or not it's a (lowercase) vowel
5. a) [1, 4, 9, 16, ..., 100]
   b) [1, 10, 20]
   c) [15, 15, 15]
6. map (\x -> bool (-x) (x) (x == 3)) [1..10]

## 9.10 Filtering lists of values

### Exercises: Filtering

1. filter (\x -> (rem x 3) == 0) [1..30]
or filter ((==0) . (flip rem 3)) [1..30]
2. length $ filter ((==0) . (flip rem 3)) [1..30]
3. filter (not . flip elem ["the", "a", "an"]) . words


## 9.11 Zipping lists

### Zipping exercises

1.
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

2. zipWith' f xs ys = map (uncurry f) (zip xs ys)
3. zipWith' f xs ys = map (uncurry f) (zip' xs ys)

## 9.12 Chapter Exercises

### Data.Char

1. isUpper :: Char -> Bool
   toUpper :: Char -> Char
2. filter isUpper
3. f (x:xs) = toUpper x : xs
4. f [] = []
   f (x:xs) = toUpper x : f xs
5. f xs = toUpper (head xs)
   f = toUpper . head
6. f :: Char -> Int -> Char
   caesar c offset = chr $ (ord 'a' +) $ flip mod 26 $ offset - ord 'a' + ord c
   uncaesar c offset = caesar c (-offset)

### Writing your own standard functions

1. myOr [] = False
   myOr (x:xs) = x || myOr xs
2. myAny f xs = myOr $ map f xs
3. myElem x = myAny (== x)
4. myReverse [] = []
   myReverse (x:xs) = myReverse xs ++ x : []
5. squish [] = []
   squish (x:xs) = x ++ squish xs
6. squishMap f = squish . map f
7. squishAgain = squishMap id
8. ???
9. ???
10. ???





   
   
   
   
   
   
   
   
   
   