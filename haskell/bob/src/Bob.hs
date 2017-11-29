module Bob (responseFor) where

import Data.Char (isUpper, isAlpha, isSpace)

responseFor :: String -> String
responseFor xs
  | isYelling xs    = "Whoa, chill out!"
  | isQuestion xs   = "Sure."
  | isWhitespace xs = "Fine. Be that way!"
  | otherwise       = "Whatever."

isYelling :: String -> Bool
isYelling xs = length alphabeticChars > 0 && all isUpper alphabeticChars
  where
    alphabeticChars = filter isAlpha xs

isQuestion :: String -> Bool
isQuestion xs = length trimmedSentence > 0 && last trimmedSentence == '?'
  where
    trimmedSentence = filter (not . isSpace) xs

isWhitespace :: String -> Bool
isWhitespace xs = filter isSpace xs == xs
