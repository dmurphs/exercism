module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = if n <= 0
  then Nothing
  else Just $ collatz' 0 n
    where
      collatz' step 1 = step
      collatz' step n =
        let
          nextNum = if n `mod` 2  == 0
            then quot n 2
            else 3 * n + 1
        in collatz' (step + 1) nextNum
