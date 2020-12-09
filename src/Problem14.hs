module Problem14
  ( solve
  ) where

import Lib
import Data.List
import Data.Function

collatz :: Int -> Int -> Int
collatz l 1 = l
collatz l n
  | n `mod` 2 == 0 = collatz (l + 1) (n `div` 2)
  | otherwise = collatz (l + 1) (3 * n + 1)

solve :: IO ()
solve = do
  print . fst . maximumBy (compare `on` snd) . zip [1..999999] . map (collatz 1) $ [1..999999]
