module ProbabilityExperiments where

import System.Random
import Control.Monad (replicateM)

{-main :: IO ()
main = do
    generators <- replicateM 100000 newStdGen
    let results = map fun generators
    let summary = (fromIntegral $ length $ filter id results)
                / (fromIntegral $ length results)
    print summary-}

main :: IO ()
main = do
    summary <- summarise 100000
    print summary

summarise :: (Fractional a) => Int -> IO a
summarise n = do
    generators <- replicateM n newStdGen
    let results = map fun generators
    let summary = (fromIntegral $ length $ filter id results)
                / (fromIntegral $ length results)
    return summary


fun :: RandomGen g => g -> Bool
fun g = let xs = randomRs (1 :: Double, 100 :: Double) g
            x = lastElementBeforeThreshold 100 xs
            y = lastElementBeforeThreshold 200 xs
        in x > y
        where  numberOfElementsBeforeThreshold threshold list = length $ takeWhile (<threshold) $ scanl (+) 0 list
               lastElementBeforeThreshold threshold list = list !! ((numberOfElementsBeforeThreshold threshold list) - 1)
  
 {-  
   let xs = randomRs (1 :: Double, 100 :: Double) g
   let n = length $ takeWhile (<100) $ scanl (+) 0 xs
   let x = xs !! (n-1)
   let n2 = length $ takeWhile (<200) $ scanl (+) 0 xs
   let y = xs !! (n2-1)
   return (x > y)
 -}