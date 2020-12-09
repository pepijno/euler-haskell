module Problem13
  ( solve,
  ) where

import Lib

solve' :: String -> String
solve' = take 10 . show . sum . map read . lines

solve :: IO ()
solve = do
  content <- readFile "inputs/13.txt"
  print $ solve' content
