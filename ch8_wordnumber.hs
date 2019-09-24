module WordNumber where

import Data.List (intersperse)


digitToWord :: Int -> String
digitToWord n
    | 0 <= n && n < 10 = digitWords !! n
    | otherwise = error "out of bounds"
         where digitWords = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits x = reverse (digitsHelper x)
    where digitsHelper 0 = []
          digitsHelper n = (flip mod 10) n
                         : (digitsHelper . flip div 10) n

wordNumber :: Int -> String
wordNumber n = concat . intersperse ", " . map digitToWord $ digits n
    
    
splitWords :: [Char] -> [[Char]]
splitWords "" = []
splitWords xs = first : splitWords rest
    where first = takeWhile cond xs
          rest = drop 1 $ dropWhile cond xs
          cond = (/= ' ')