module Queens (boardString, canAttack) where

import Data.List (intersperse, unlines)

maxIndex :: Int
maxIndex = 7

isPositionMatch :: (Int, Int) -> Maybe (Int, Int) -> Bool
isPositionMatch (row, col) player = case player of
  Just (playerRow, playerCol) -> row == playerRow && col == playerCol
  Nothing                     -> False

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ map getRow [0..maxIndex]
  where
    getPositionChar position
      | isPositionMatch position white = 'W'
      | isPositionMatch position black = 'B'
      | otherwise                      = '_'
    getRow rowNum = intersperse ' ' $ map (getPositionChar . (,) rowNum) [0..maxIndex]

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (aRow, aCol) (bRow, bCol) =
  aRow == bRow
  || aCol == bCol
  || abs (bRow - aRow) == abs (bCol - aCol)
