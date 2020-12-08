module Problem2
  ( solve
  ) where

import Lib

fib :: [Int]
fib = 1:1:(zipWith (+) fib (tail fib))

solve :: Int
solve = sum . filter even $ takeWhile (<=4000000) fib
