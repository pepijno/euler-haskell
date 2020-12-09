module Problem12
  ( solve,
  numberOfDivisors,
  triangulars
  ) where

import Lib
import qualified Data.Map as Map

numberOfDivisors :: Int -> Int
numberOfDivisors x = Map.foldl (*) 1 . Map.map (+1) $ primeFactors x

triangular :: Int -> Int
triangular x = x * (x + 1) `div` 2

triangulars :: [Int]
triangulars = map triangular [1..]

solve :: IO ()
solve = do
  print . triangular . (+) 1 . length . takeWhile (<500) . map numberOfDivisors $ triangulars
