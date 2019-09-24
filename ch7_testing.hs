module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main :: IO ()
main = do
    print ((roundTrip 4) :: Double)
    print (id 4)
    

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < 0 = negate $ go (-n) d count
           | d < 0 = negate $ go n (-d) count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)
          negate (a, b) = (-a, b)