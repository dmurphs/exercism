module DNA (nucleotideCounts) where

import Data.Map (Map, fromListWith, keysSet)
import qualified Data.Set as S (fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs = if S.fromList nucleotides == keysSet frequencies
  then Right frequencies
  else Left "Sequence contains non nucleotide character"
  where
    frequencies = fromListWith (+) (baseFrequencies ++ [(x,1) | x <- xs])
    baseFrequencies = [(x,0) | x <- nucleotides]
    nucleotides = "ACGT"

-- The following is an alternate implementation, in general it performs worse,
-- however it does do well in the case where there is 'bad' nucleotide early on
-- as it will not continue recursion.

-- nucleotideCounts :: String -> Either String (Map Char Int)
-- nucleotideCounts []     = Right $ fromList [(x,0) | x <- nucleotides]
-- nucleotideCounts (x:xs) = if elem x nucleotides
--   then liftM2 (unionWith (+)) (Right (fromList [(x,1)])) (nucleotideCounts xs)
--   else Left "Encountered non-nucleotide character"
