# Chapter 7

## 7.3 Anonymous functions

### Exercises: Grab Bag

1. I thnk they're all equivalent?
2. d
3. a) where f = \x -> x + 1
   b) addFive = \x -> \y -> (if x > y then y else x) + 5
   c) mflip f x y = f y x


## 7.4 Pattern Matching

### Exercises: Variety Pack

1. a) k :: (a, b) -> a
   b) `[Char]`, no; those are Num a => a
   c) k1, k3
   
2. f (a, b, c) (d, e, f) = ((a, d), (c, f))

## 7.5 Case expressions

### Exercises: Case Practice

1. functionC x y = case (x > y) of True -> x; False -> y
2. ifEvenAdd2 n = case even n of True -> n+2; False -> n
3. nums x = case compare x 0 of LT -> -1; GT -> 1; EQ -> 0

## 7.6 Higher order functions

### Exercises: Artful Dodgy

2. 11
3. 22
4. 21
5. 12
6. 11
7. 21
8. 21
9. 22
10. 31
11. 23

## 7.7 Guards

### Exercises: Guard Duty

1. I get a syntax error, I think
3. b
4. `[Char]`
5. `[Char]` -> Bool
6. c
7. Num a => a
8. (Num a, Num b) => a -> b

## 7.11 Chapter exercises

### Multiple Choice

1. d
2. b
3. d
4. b
5. a

### Let's write code

1. 
a)
tensDigit x = d
    where xLast = fst (x `divMod` 10)
          d = snd (xLast `divMod` 10)
b) Yup
c)
hunsD x = d2
    where d = fst (x `divMod` 100)
          d2 = snd (2 `divMod` 100)
-- or
hunsD = (snd . flip divMod 10 . fst . flip divMod 10)

2.

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

foldBool1 x y b = case b of
    True -> x
    False -> y

foldBool2 x y b
    | b = x
    | otherwise = y

3. g f (a, c) = (f a, c)
4. done
5. roundTrip = read . show
6. roundTrip :: (Show a, Read b) => a -> b
   print ((roundTrip 4) :: Double)
   
