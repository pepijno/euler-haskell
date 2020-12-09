module Problem12
  ( solve,
  numberOfDivisors,
  triangulars
  ) where

import Lib
import qualified Data.Map as M

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

numberOfDivisors :: Int -> Int
numberOfDivisors x = M.foldl (*) 1 . M.map (+1) $ primeFactors x

triangular :: Int -> Int
triangular x = x * (x + 1) `div` 2

triangulars :: [Int]
triangulars = map triangular [1..]

solve :: Int
solve = triangular . (+) 1 . length . takeWhile (<500) . map numberOfDivisors $ triangulars
