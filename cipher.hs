module Chipher where

import Data.Char

caesar :: Char -> Int -> Char
caesar c offset = chr $ (ord 'a' +) $ flip mod 26 $ ord c - ord 'a' + offset
uncaesar c offset = caesar c (-offset)

caesar' :: Char -> Char -> Char
caesar' c offset = caesar c $ ord offset - ord 'a'
uncaesar' c offset = uncaesar c $ ord offset - ord 'a'

viginere :: String -> String -> String
viginere secret key = map (uncurry caesar') $ zip secret $ concat $ repeat key