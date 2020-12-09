module Problem17
  ( solve,
  splitNumber,
  numberToWords'
  ) where

import Lib
import Data.List

ones :: Int -> String
ones n 
  | n > 0 && n < 10 = onsies !! (n - 1)
  | otherwise = ""
  where
    onsies = words "one two three four five six seven eight nine"

teens :: Int -> String
teens n 
  | n >= 10 && n < 20 = teensies !! (n - 10)
  | otherwise = ""
  where
    teensies = words "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"

tens :: Int -> String
tens n 
  | n > 0 && n < 10 = tensies !! (n - 1)
  | otherwise = ""
  where
    tensies = words "ten twenty thirty fourty fifty sixty seventy eighty ninety"

groups :: [String]
groups = ["hundred", "thousand"]

splitNumber :: Int -> [Int]
splitNumber 0 = []
splitNumber n = let (d, r) = n `quotRem` 10
                 in r : splitNumber d

numberToWords' :: [Int] -> String
numberToWords' [] = "zero"
numberToWords' [x] = ones x
numberToWords' (x:y:rest) = intercalate "and" . filter (not . null) $ hundreds ++ ts
  where ts = [if y == 1 then teens (x + 10 * y) else (tens y) ++ (ones x)]
        hundreds = map (\(a,b) -> (ones a) ++ b) . reverse . filter ((/=) 0 . fst) $ zip rest groups

numberToWords :: Int -> String
numberToWords = numberToWords' . splitNumber

solve :: IO ()
solve = do
  print . sum . map (length . numberToWords) $ [1..1000]
