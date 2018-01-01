module RotationalCipher (rotate) where

import Data.Char
import Data.List

alphabet :: String
alphabet = ['a'..'z']

rotate :: Int -> String -> String
rotate n text = map (encodeChar n) text

encodeChar :: Int -> Char -> Char
encodeChar n c = case fmap shift (getIndex lowerC) of
  Just i  -> if (isUpper c)
    then toUpper $ alphabet !! i
    else alphabet !! i
  _       -> c
  where
    lowerC = toLower c
    shift = ((flip mod) 26) . (+ n)

getIndex :: Char -> Maybe Int
getIndex = (flip elemIndex) alphabet
