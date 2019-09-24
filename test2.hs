module Test2 where

triple x = x * 3

-- let foo x =
-- 	let y = x * 2
--		z = x ^ 2
--	in 2 * y * z

printInc n = print plusTwo
    where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

mult1 = x * y
    where x = 5
          y = 6

main :: IO ()
main = do
    putStrLn "Count to four for me:"
    putStr "one, two"
    putStr ", three, and"
    putStrLn " four!"