module Problem28
  ( solve
  ) where

import Lib

solve :: IO ()
solve = do
  print $ sum $ takeWhile (<= 1001 * 1001) $ scanl (+) 1 $ map (\x -> 2 * (1 + ((x - 1) `div` 4))) [1..(1001 * 1001)]
