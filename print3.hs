module Print3 where

myGreeting :: String
myGreeting = "hello" ++ "world"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = 
            concat [hello, " ", world]
            
            
-- 3.8 CHAPTER EXCERCISES pg 81

{- Reading Syntax
1.
a) good
b) ++ should be in parens
c) good
d) ++ should be comma
e) should be "hello" !! 4
f) good
g) should be 4 "lovely" not "4 lovely"
h) good


2.
a -- d
b -- c
c -- e
d -- a
e -- b

-}

{- Building Functions

1.
a) (++ "!")
b) (!! 4)
c) drop 9

2. same as a

3.
thirdLetter :: String -> Char
thirdLetter x = x !! 2

4.
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

5.
rvrs :: String -> String
rvrs x = drop 9 x ++ take 4 (drop 5 x) ++ take 5 x

6.

main :: IO ()
main = print $ rvrs "Curry is awesome"

-}