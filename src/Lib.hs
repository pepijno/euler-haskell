module Lib
  ( primeFactors
  ) where

import qualified Data.Map as Map

factorOut :: Int -> Int -> (Int, Int)
factorOut divisor x
  | r == 0 = (rest, n + 1)
  | otherwise = (x, 0)
  where (d, r) = x `quotRem` divisor
        (rest, n) = factorOut divisor d

(?:) :: (Int, Int) -> Map.Map Int Int -> Map.Map Int Int
(?:) (_, 0) rest = rest
(?:) (x, y) rest = Map.insert x y rest

primeFactors' :: Int -> Int -> Map.Map Int Int
primeFactors' _ 1 = Map.empty
primeFactors' prime x = (prime, n) ?: primeFactors' (prime + 2) v
  where (v, n) = factorOut prime x

primeFactors :: Int -> Map.Map Int Int
primeFactors x = (2, n) ?: primeFactors' 3 v
  where (v, n) = factorOut 2 x
