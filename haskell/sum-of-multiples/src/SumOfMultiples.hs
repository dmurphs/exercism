module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as S

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ S.fromList multiplesOfFactors
  where
    multiplesOfFactors = concat $ map multiplesToLimit factors
    multiplesToLimit n = takeWhile (< limit) $ map (* n) [1..]
