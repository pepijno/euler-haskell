{-# LANGUAGE TupleSections #-}

module Problem23
  ( solve
  ) where

import Lib
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

divisors :: Int -> Map.Map Int Int
divisors n = foldr unionSumMultiples initial [2..n]
  where initial = Map.fromList $ (,1) <$> [1..n]
        unionSumMultiples xs values = Map.unionWith (+) values $ Map.fromList $ (,xs) <$> [2*xs,3*xs..n]

abundants :: [Int]
abundants = map fst $ filter (uncurry (<)) $ Map.toList $ divisors 28123

listOfNoSum :: [Int]
listOfNoSum = filter (\x -> all (\y -> not $ Set.member (x - y) abundantsSet) $ takeWhile (< x) abundants) [1..28123]
  where abundantsSet = Set.fromList abundants

solve :: IO ()
solve = do
  print $ sum listOfNoSum
