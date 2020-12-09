module Problem10
  ( solve
  ) where

import Lib

isPrime :: Int -> Bool
isPrime 2 = True
isPrime x = all (\d -> x `mod` d /= 0) $ 2:[3,5..max]
  where max = floor . sqrt $ fromIntegral x

primes :: [Int]
primes = 2:[x | x <- [3,5..], isPrime x]

solve :: Int
solve = sum $ takeWhile ((>) 2000000) primes
