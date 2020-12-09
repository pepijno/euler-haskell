module Problem5
  ( solve
  ) where

import Lib

solve :: IO ()
solve = do
  print $ foldl lcm 1 [1..20]
