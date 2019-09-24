module Chapter4 where

-- 4.3 Anatomy of a data declaration

{- Exercises: Mood Swing

1. Mood
2. Blah and Woot
3. Woot isn't a type, we should be returning Mood. That's especially important for allowing us to return Blah in the Woot case.
-}

-- 4/5:
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah


-- 4.6 Go on and Bool me

{- Exercises: Find the Mistakes

1. not True && True
2. not (x == 6)
3. good as is
4. "Merry" > "Happy"
5. a list of chars and a list of ints cannot be meaningfully combined

-}



-- 4.9 Chapter Excercises 

{- Unnamed first part

1. length :: [a] -> Integer, takes a single list and returns an Integer
2. a) 5  b) 3  c) 2  d) 5
3. 6/3 should work because 3 is just a Num and can be Fractional, but 6 / length [1, 2, 3] won't
because length returns an Integer, which is not Fractional.
4. div should work because it only accepts Integrals. 6 `div` length [1, 2, 3]
5. Bool, True
6. Bool, False

7. 
a) Yes, True
b) No, mixed types in lists are banned
c) Yes, 5
d) Yes, False
e) No, 9 is not a Bool

8. isPalindrome x = x == reverse x
9. myAbs x = if x >= 0 then x else -x
10. f (a, b) (c, d) = ((b, d), (a, c))

-}

{- Correcting Syntax

1.
x = (+)
f xs = w `x` 1
    where w = length xs
    
2. Not sure what this \ notation is supposed to be? No answer

3. f (a b) = a

-}

{- Match the function names to their types

1. idk, we haven't really talked about show at all. I think it might be a? Update: nope, c
2. b
3. a
4. d

-}