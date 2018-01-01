module PerfectNumbers (classify, Classification(..)) where

import Control.Monad

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0         = Nothing
  | n == 1         = Just Deficient
  | n > factorSum  = Just Deficient
  | n < factorSum  = Just Abundant
  | n == factorSum = Just Perfect
  where
    factorSum = sum factors
    factors = factorsUpToSqrtFloor ++ map (quot n) filteredForDuals
    filteredForDuals = filter dualFilter factorsUpToSqrtFloor
    dualFilter = if sqrtFloorN^2 == n
      then (liftM2 (&&) (> 1) (/= sqrtFloorN))
      else (> 1)
    factorsUpToSqrtFloor = [i | i <- [1..sqrtFloorN], n `mod` i == 0]
    sqrtFloorN = (floor . sqrt . fromIntegral) n
