module Problem26
  ( solve
  ) where

import Lib
import Data.List
import Data.Function
import qualified Data.Map.Strict as M

divide :: Int -> [Int] -> M.Map Int Int -> Int -> [Int]
divide 0 _ _ _ = []
divide x xs m p = case M.lookup x m of
                    Just k -> drop k xs
                    Nothing -> let (d, r) = (10 * x) `quotRem` p
                                in divide r (xs ++ [d]) (M.insert x (length xs) m) p

solve :: IO ()
solve = do
  print . fst . last . sortBy (compare `on` snd) . zip [2..1000] . map (length . divide 1 [] M.empty) $ [2..1000]
