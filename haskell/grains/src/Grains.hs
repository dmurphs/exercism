module Grains (square, total) where

square :: Integer -> Maybe Integer
square n = if n <= 0 || n > 64
  then Nothing
  else Just (2^(n-1))

-- This is the just geometric series with ratio 2 and starting term 1 from 0 to 63
-- We can just used the closed form formula
total :: Integer
total = quot (1 - 2^64) (-1)
