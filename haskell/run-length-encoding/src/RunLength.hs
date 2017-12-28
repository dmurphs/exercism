module RunLength (decode, encode) where

import Control.Applicative
import Text.Trifecta

decode :: String -> String
decode encodedText = case (parser encodedText) of
  Success decoded -> decoded
  _               -> "encoded text is malformed"
  where parser = parseString decodeParser mempty

encode :: String -> String
encode text = error "You need to implement this function."

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
