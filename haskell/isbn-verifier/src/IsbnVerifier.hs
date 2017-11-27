module IsbnVerifier (isbn) where

import Data.Char
import Data.List.Split

isbn :: String -> Bool
isbn = checkValidIsbn . parseIsbn

parseIsbn :: String -> [Maybe Int]
parseIsbn = (fmap getIsbnDigit) . (filter (\c -> c /= '-'))

getIsbnDigit :: Char -> Maybe Int
getIsbnDigit c
  | c == 'X'  = Just 10
  | isDigit c = Just $ digitToInt c
  | otherwise = Nothing

isbnFormula :: [Maybe Int] -> Int
isbnFormula concatenatedIsbn =
  foldr (+) 0 $ zipWith (*) (reverse [1..10]) isbnDigits
  where
    isbnDigits = (fmap mapToDigit concatenatedIsbn)

mapToDigit :: Maybe Int -> Int
mapToDigit d = case d of
  Just n  -> n
  Nothing -> 0

xOnlyCheck :: [Maybe Int] -> Bool
xOnlyCheck concatenatedIsbn =
  not (elem (Just 10) nonCheckItems)
  where
    nonCheckItems = drop 1 (reverse concatenatedIsbn)

checkValidIsbn :: [Maybe Int] -> Bool
checkValidIsbn concatenatedIsbn =
    not (elem Nothing concatenatedIsbn)
    && xOnlyCheck concatenatedIsbn
    && length concatenatedIsbn == 10
    && (isbnFormula concatenatedIsbn) `mod` 11 == 0
