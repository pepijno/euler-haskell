module Problem14
  ( solve
  ) where

import Lib
import Data.List
import Data.Function

collatz :: Int -> Int -> Int
collatz l 1 = l
collatz l n
  | d == 0 = collatz (l + 1) r
  | otherwise = collatz (l + 1) (3 * n + 1)
  where (r, d) = n `quotRem` 2

solve :: IO ()
solve = do
  print . fst . maximumBy (compare `on` snd) . zip [1..999999] . map (collatz 1) $ [1..999999]
