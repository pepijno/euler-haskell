module Problem5
  ( solve
  ) where

import Lib

solve :: Int
solve = foldl lcm 1 [1..20]
