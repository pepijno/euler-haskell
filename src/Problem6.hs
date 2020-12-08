module Problem6
  ( solve
  ) where

import Lib

sumOfSquares :: Int -> Int
sumOfSquares x = sum . map (^2) $ [1..x]

squareOfSums :: Int -> Int
squareOfSums x = (^2) $ sum [1..x]

solve :: Int
solve = squareOfSums 100 - sumOfSquares 100
