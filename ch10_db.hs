module Database where

import Data.Time

data DatabaseItem =
      DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
      , DbNumber 9001
      , DbString "Hello, world!"
      , DbDate (UTCTime
               (fromGregorian 1921 5 1)
               (secondsToDiffTime 34123))
    ]
   
     
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where f (DbDate time) x = time : x
          f _ x = x
          
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where f (DbNumber num) x = num : x
          f _ x = x

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = head . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = (fromIntegral $ sum l) / (fromIntegral $ length l)
    where l = filterDbNumber x
    
    