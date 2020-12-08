module Problem3
  ( solve,
  ) where

import Lib
import Data.Maybe
import qualified Data.Map.Strict as M

factorOut :: Int -> Int -> (Int, Int)
factorOut divisor x
  | x `mod` divisor == 0 = (rest, n + 1)
  | otherwise = (x, 0)
    where (rest, n) = factorOut divisor (x `div` divisor)

(?:) :: (Int, Int) -> M.Map Int Int -> M.Map Int Int
(?:) (_, 0) rest = rest
(?:) (x, y) rest = M.insert x y rest

primeFactors' :: Int -> Int -> M.Map Int Int
primeFactors' _ 1 = M.empty
primeFactors' prime x = (prime, n) ?: primeFactors' (prime + 2) v
  where (v, n) = factorOut prime x

primeFactors :: Int -> M.Map Int Int
primeFactors x = (2, n) ?: primeFactors' 3 v
  where (v, n) = factorOut 2 x

solve :: Int
solve = fst $ fromJust $ M.lookupMax $ primeFactors 600851475143
