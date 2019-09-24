# Chapter 8: Functions that call themselves

## 8.6 Chapter exercises

### Review of types

1. d
2. b
3. d
4. b

### Reviewing currying

1. "whoops mrow woohoo!"
2. "1 mrow haha"
3. "whoops mrow 2 mrow haha"
4. "woops mrow blue mrow haha"
5. "pink mrow haha mrow green mrow woops mrow blue"
6. "are mrow Pugs mrow awesome"

### Recursion

1. No
2. addUp 0 = 0
   addUp n = n + addUp (n-1)
3. times x 0 = 0
   times x y = x + times x (y-1)

### Fixing dividedBy

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < 0 = negate $ go (-n) d count
           | d < 0 = negate $ go n (-d) count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)
          negate (a, b) = (-a, b)

### McCarthy 91 function

mc n
    | n > 100 = n - 10
    | otherwise = mc . mc $ n + 11

### Numbers into words

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

