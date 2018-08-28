module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size = [[getNumber i j| j <- indices] | i <- indices]
  where
    indices = [0..(size-1)]
    getNumber i j
      | i <= j    = start + iDepthAdjusted + jDepthAdjusted + 1
      | otherwise = nextStart - iDepthAdjusted - jDepthAdjusted + 1
      where
        -- Get index in context of square at current depth
        iDepthAdjusted = i - depth
        jDepthAdjusted = j - depth
        -- starting numbers of square at next and current depth
        nextStart = start + numPositionsInSquare
        start = sum $ map getNumPositionsInSquare [0..(depth-1)]
        numPositionsInSquare = getNumPositionsInSquare depth
        -- Depth of current square
        depth = min iDepth jDepth
        iDepth = min (maxIndex - i) i
        jDepth = min (maxIndex - j) j
        maxIndex = size - 1
    getNumPositionsInSquare depth = 4 * ((size - (depth * 2)) - 1)

