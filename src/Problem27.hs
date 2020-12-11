module Problem27
  ( solve,
  countPrimes
  ) where

import Lib
import Data.List
import Data.Function

isPrime :: Int -> Bool
isPrime 2 = True
isPrime x
  | x < 0 = False
  | otherwise = all (\d -> x `mod` d /= 0) $ 2:[3,5..max]
  where max = floor . sqrt $ fromIntegral x

quadratic :: Int -> Int -> Int -> Int
quadratic a b x = x * x + a * x + b

countPrimes :: Int -> Int -> Int
countPrimes a b = length . takeWhile (isPrime) $ map (quadratic a b) [0..]

findMax :: Int
findMax = let (x, y) = fst $ last $ sortBy (compare `on` snd) $ zip xs $ map (uncurry countPrimes) xs
           in x * y
             where xs = [(a,b) | a <- [(-999)..999], b <- [(-1000)..1000]]

solve :: IO ()
solve = do
  print $ findMax
