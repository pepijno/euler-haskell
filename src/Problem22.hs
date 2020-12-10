module Problem22
  ( solve
  ) where

import Lib
import Data.Char
import Data.List

nameToValue :: String -> Int
nameToValue = sum . map toDigit
  where toDigit x = (ord x) - (ord 'A') + 1

solve :: IO ()
solve = do
  names <- readFile "inputs/22.txt"
  print . sum . zipWith (*) [1..] . map nameToValue . sort $ words names
