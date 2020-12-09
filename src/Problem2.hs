module Problem2
  ( solve
  ) where

import Lib

fib :: [Int]
fib = 1:1:(zipWith (+) fib (tail fib))

solve :: IO ()
solve = do
  print . sum . filter even $ takeWhile (<=4000000) fib
