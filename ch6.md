## Chapter 6


### 6.5 Writing typeclass instances

#### Exercises: Eq Instances

1. instance Eq (TisAnInteger) where
        (==) (TisAn x) (TisAn y) = x == y
2. instance Eq (TwoIntegers) where
        (Two x y) == (Two x' y') = (x == x') && (y == y')
3. instance Eq (StringOrInt) where
        (TisAnInt x) == (TisAnInt x') = x == x'
        (TisAString x) == (TisAString x') = x == x'
        _ == _ = False
4. instance Eq a => Eq (Pair a) where
        (Pair x y) == (Pair x' y') = (x == x') && (y == y')
    -- possibly; I get a weird error
5. instance (Eq a, Eq b) => Eq (Tuple a  b) where
        (Tuple x y) == (Tuple x' y') = (x == x') && (y == y')
6. instance Eq a => Eq (Which a) where
        ThisOne x == ThisOne x' = x == x'
        ThatOne x == ThatOne x' = x == x'
        _ == _ = False
7. instance (Eq a, Eq b) => Eq (EitherOr a b) where
        First x == First x' = x == x'
        Second x == Second x' = x == x'
        _ == _ = False
        
### 6.8 Ord

#### Exercises: Will They Work?

1. Yup, 'length' returns Ints, and those can be compared just fine
2. Yes, but I'm not totally sure why. (3*4) and (3*5) are both guaranteed to be Nums, but Num doesn't require Ord. I guess maybe because Nums default to Integer and Integer implements Ord?
3. Nope, Bool and `[Char]` can't be compared
4. Yeah, see 2

### 6.9 Enum

Enum is kinda confusing, not sure exactly what the point is
succ (1.5 :: Double) is 2.5 which is very counterintuitive
    (should be 1.5000000xx or something, like the next value that Double can represent u feel?)
    Allele says yes, this is kinda weird and nuanced
    
### 6.10 Show

IO is weird

### 6.14 Chapter Exercises

#### Multiple Choice
 
1. c
2. a
3. a
4. c, I think?
    Checked: yes
5. a

#### Does it typecheck?

1. No, Person did not implement or derive Show
2. I think it should?
    check: Nope, I forgot that Mood didn't derive Eq
3. (typechecked by adding deriving Eq)
   a) only Blah and Woot
   b) typecheck fail, 9 is not a Mood
   c) typecheck fail, Mood still doesn't implement Ord
4. s1 is too short, s2 works

#### Given a datatype declaration, what can we do?

1. Nope, need to use the data constructors
2. Yep
3. Yep
4. Nope, Papu doesn't implement Ord

#### Match the types

I don't really understand the question here? pg. 209

#### Type-Kwon-Do Two: Electric Typealoo

1. chk f x y = f x == y
2. arith f x y = fromInteger x + f y
