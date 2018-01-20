module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, fromList, toList)

transform :: Map a String -> Map Char a
transform = fromList . concatMap invertKeyValuePair . toList
  where
    invertKeyValuePair (k,v) = map (\c -> (toLower c,k)) v
