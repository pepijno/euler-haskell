module Problem18
  ( solve,
  calcMaxPascal'
  ) where

import Lib

calcMaxPascal' :: [Int] -> [[Int]] -> [Int]
calcMaxPascal' prev [] = prev
calcMaxPascal' prev (x:xs) = calcMaxPascal' prev' xs
  where left = zipWith (+) (prev ++ [0]) x
        right = zipWith (+) (0:prev) x
        prev' = zipWith max left right

solve :: IO ()
solve = do
  contents <- readFile "inputs/18.txt"
  -- print . maximum . foldl calcMaxPascal' [] . map (map read . words) . lines $ contents
  print . maximum . calcMaxPascal' [] . map (map read . words) . lines $ contents
