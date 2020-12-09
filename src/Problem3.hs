module Problem3
  ( solve,
  ) where

import Lib
import Data.Maybe
import qualified Data.Map.Strict as Map

solve :: IO ()
solve = do
  print $ fst $ fromJust $ Map.lookupMax $ primeFactors 600851475143
