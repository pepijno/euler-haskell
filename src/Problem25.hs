module Problem25
  ( solve
  ) where

import Lib

fibs :: [Integer]
fibs = 0:1:rest
  where rest = zipWith (+) (tail fibs) fibs

solve :: IO ()
solve = do
  print . length . takeWhile (< 10^999) $ fibs
