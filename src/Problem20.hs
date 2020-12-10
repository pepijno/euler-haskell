module Problem20
  ( solve,
  factorial
  ) where

import Lib
import Data.Char

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = product [1..x]

solve :: IO ()
solve = do
  print . sum . map digitToInt . show $ factorial 100
