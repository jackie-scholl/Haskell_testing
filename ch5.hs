module Chapter5 where

-- 5.3 How to read type signatures

{- Exercises: Type Matching

a -- c
b -- d
c -- b
d -- a
e -- e

-}

-- 5.4 Currying

{- Exercises: Type arguments

1. a
2. d
3. d
4. c
5. a
6. e
7. d
8. a
9. c

-}

-- 5.5 Polymorphism

{- Exercises: Parametricity

1. Not possible

2.
let f :: a -> a -> a; f x y = x
let g :: a -> a -> a; g x y = x

3. f x y = y is the only possibility in this general form

-}

-- 5.6 Type Inference

{- Exercises: Apply yourself

I don't really understand what these exercises are asking for?

-}

-- 5.8 Chapter Exercises

{- Exercises: Multiple choice

1. c
2. a
3. b
4. c
-}

{- Exercises: Determine the Type

1.
a) 54; Num a => a
b) (0, "doge"); (Num a) => (a, [Char])
c) (0, "doge"); (Integer, [Char])
d) False; Bool
e) 5; Int
f) False; Bool

2. Num a => a
3. Num a => a -> a
4. Fractional a => a -> a
5. [Char]

-}

{- Exercises: Does it compile?

1. bigNum is a constant, not a function, and it cannot be applied. wahoo = bigNum
2. Works fine
3. In c, b is not a function, you want a here
4. c is not defined

-}

{- Type variable or specific type constructor?

2. fully polymorphic; concrete; concrete
3. fully polymorphic; constrained polymorphic; concrete
4. fully polymorphic; fully polymorphic; concrete (I think; but maybe f the function and f the type variable interfere? idk)
-}

{- Write a type signature

1. [a] -> a
2. Ord a => a -> a -> Bool
3. (a, b) -> b
-}

{- Given a type, write the function

1. i x = x
2. c a b = a
3. Yes
4. c a b = b
5. r = take 3
6. co g f a = g (f a)
7. a _ x = x

-}

{- Fix it

1. module sing becomes module Sing, [Char] ++ [Char] becomes [Char] -> [Char],  [Char] -> Char becomes  [Char] -> [Char], second x= becomes a y=
2. (x > y) becomes (x < y)
3. Main becomes main, first print gets parentheses around 1+2, putStrLn becomes print, -1 gets parentheses

-}

{- Type-Kwon-Do

1. h x = g (f x)
2. e x = w (q x)
3. xform (x, y) = (xz x, yz y)
4. munge f g x = fst (g (f x))

-}