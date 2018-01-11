module Diamond (diamond) where

diamond :: Char -> [String]
diamond c = case chars of
  [] -> []
  _  -> top ++ (tail $ reverse top)
  where
    chars = ['A'..c]
    top = map diamondRow chars
    diamondRow c' =
      let right = map (\a -> if a == c' then a else ' ') chars
      in (reverse $ tail right) ++ right
