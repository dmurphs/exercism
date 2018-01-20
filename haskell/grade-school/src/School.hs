module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import Data.List

type School = M.Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student = M.insertWith (flip (++)) gradeNum [student]

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade = M.findWithDefault []

sorted :: School -> [(Int, [String])]
sorted = (fmap . fmap) sort . M.toAscList
