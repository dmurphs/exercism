module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = checkPangram (map toLower text) ['a'..'z']

checkPangram :: String -> String -> Bool
checkPangram text [] = True
checkPangram text (c:xs) =
  elem c text && checkPangram text xs
