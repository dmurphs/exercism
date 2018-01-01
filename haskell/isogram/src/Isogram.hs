module Isogram (isIsogram) where

import Control.Monad
import Data.Char
import Data.List

isIsogram :: String -> Bool
isIsogram = liftM2 (==) (length . processChars) (length . nub . processChars)
  where processChars = (map toLower) . (filter (not . (flip elem) "- "))
