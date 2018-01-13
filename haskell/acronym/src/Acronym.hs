module Acronym (abbreviate) where

import Data.Char (toUpper)
import Text.Trifecta

abbreviate :: String -> String
abbreviate xs = case parser xs of
  Success parsed -> map (toUpper . head) parsed
  _              -> "Error parsing given text"
  where parser = parseString parseWords mempty

parseWords :: Parser [String]
parseWords = do
  wds <- many parseWord
  return wds

parseWord :: Parser String
parseWord = do
  many $ noneOf (['a'..'z'] ++ ['A'..'Z'])
  startUpper <- many upper
  remaining <- (case startUpper of
    [] -> some
    _  -> many) $ noneOf ('-':' ':['A'..'Z'])
  return (startUpper ++ remaining)
