module RunLength (decode, encode) where

import Control.Applicative
import Text.Trifecta

-- DECODER

decode :: String -> String
decode encodedText = case (parser encodedText) of
  Success decoded -> decoded
  _               -> "encoded text is malformed"
  where parser = parseString decodeParser mempty

type IntegerOrChar = Either Integer Char

decodeCharParser :: Parser String
decodeCharParser = do
  firstComponent <- (Left <$> integer') <|> (Right <$> anyChar)
  case firstComponent of
    Left number -> do
      c <- anyChar
      return $ [ c | _ <- [1..number]]
    Right c  -> return [c]

decodeParser :: Parser String
decodeParser = do
  expandedChars <- many decodeCharParser
  return $ concat expandedChars

-- ENCODER

encode :: String -> String
encode "" = ""
encode (x:xs) = encode' (xs) (x, 1)

-- encodeCharacterRun :: Char -> Parser String
-- encodeCharacterRun c = do
--   run <- many $ char c
--   return $ case run of
--     "" -> ""
--     [c] -> [c]
--     (x:xs) -> (show $ length run) ++ [c]

encode' :: String -> (Char,Int) -> String
encode' "" runData = encodeRun runData
encode' (nextChar:remaining) (currentChar,count) = if nextChar == currentChar
  then encode' remaining (nextChar,count+1)
  else (encodeRun (currentChar,count)) ++ (encode' remaining (nextChar,1))

encodeRun :: (Char,Int) -> String
encodeRun (c,count) = case count of
  0 -> ""
  1 -> [c]
  _ -> show count ++ [c]
