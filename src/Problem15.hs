module Problem15
  ( solve
  ) where

import Lib
import Math.Combinatorics.Exact.Binomial

solve :: IO ()
solve = do
  print $ choose 40 20
