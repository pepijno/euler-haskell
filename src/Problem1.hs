module Problem1
  ( solve
  ) where

import Lib

solve :: Int
solve = sum . filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) $ [1..999]
