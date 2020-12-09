module Problem4
  ( solve
  ) where

import Lib

isPalindrome :: Int -> Bool
isPalindrome x = reverse s == s
  where s = show x

solve :: IO ()
solve = do
  print $ maximum [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]
