module Problem16
  ( solve
  ) where

import Lib
import Data.Char

solve :: IO ()
solve = do
  print . sum . map digitToInt . show $ 2^1000
