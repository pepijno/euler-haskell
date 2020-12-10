{-# LANGUAGE TupleSections #-}

module Problem21
  ( solve
  ) where

import Lib
import qualified Data.Map.Strict as Map

divisors :: Int -> Map.Map Int Int
divisors n = foldr unionSumMultiples initial [2..n]
  where initial = Map.fromList $ (,1) <$> [1..n]
        unionSumMultiples xs values = Map.unionWith (+) values $ Map.fromList $ (,xs) <$> [2*xs,3*xs..n]

isAmicable :: Map.Map Int Int -> Int -> Bool
isAmicable m d = (d /= d') && (d == d'')
  where d' = Map.findWithDefault 1 d m
        d'' = Map.findWithDefault 1 d' m

solve :: IO ()
solve = do
  print . sum . Map.keys . Map.filterWithKey (\k _ -> isAmicable div k) $ div
    where div = divisors 10000
