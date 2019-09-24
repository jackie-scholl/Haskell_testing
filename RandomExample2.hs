module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample


rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))


rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie


nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen


rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= n = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen



rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g =
    let xs = go 0 [] g
    in (length xs, xs)
    where
        go :: Int -> [Die] -> StdGen -> [Die]
        go sum log gen
            | sum >= n = log
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (log ++ [intToDie die]) nextGen


rollsCountLogged' :: Int -> StdGen -> (Int, [Die])
rollsCountLogged' n g =
    let xs = go [] g
    in (length xs, intToDie <$> xs)
    where
        go :: [Int] -> StdGen -> [Int]
        go log gen
            | sum log >= n = log
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (log ++ [die]) nextGen



