module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = traverse rnaComplement
  where
    rnaComplement nucleotide = case nucleotide of
      'G' -> Just 'C'
      'C' -> Just 'G'
      'T' -> Just 'A'
      'A' -> Just 'U'
      _   -> Nothing
