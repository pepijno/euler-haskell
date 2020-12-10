module Problem24
  ( solve
  ) where

import Lib
import Data.List

solve :: IO ()
solve = do
  print . foldl1 (++) . map show . flip (!!) 999999 . sort . permutations $ [0..9]
