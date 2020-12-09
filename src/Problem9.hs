module Problem9
  ( solve
  ) where

import Lib

solve :: Int
solve = head [a * b * c | a <- [1..1000], b <- [1..1000], c <- [1..1000], a^2 + b^2 == c^2, a + b + c == 1000]
