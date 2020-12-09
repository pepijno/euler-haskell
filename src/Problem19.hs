module Problem19
  ( solve
  ) where

import Lib
import Data.List
import Data.Function

isLeapYear :: Int -> Bool
isLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4 == 0 = True
  | otherwise = False

nrDaysOfMonth :: Int -> Int -> Int
nrDaysOfMonth 1 year = if isLeapYear year then 29 else 28
nrDaysOfMonth 3 _ = 30
nrDaysOfMonth 5 _ = 30
nrDaysOfMonth 8 _ = 30
nrDaysOfMonth 10 _ = 30
nrDaysOfMonth _ _ = 31

incrementDays :: Int -> (Int, Int) -> Int
incrementDays day (month, year) = (day + (nrDaysOfMonth month year)) `mod` 7

solve :: IO ()
solve = do
  print . length . filter (==6) . drop 12 . scanl incrementDays 0 . sortBy (compare `on` snd) $ [(month, year) | month <- [0..11], year <- [1900..2000]]
