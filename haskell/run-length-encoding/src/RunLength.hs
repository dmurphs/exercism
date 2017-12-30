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

decodeCharacterRun :: Parser String
decodeCharacterRun = do
  firstComponent <- (Left <$> integer') <|> (Right <$> anyChar)
  case firstComponent of
    Left number -> do
      c <- anyChar
      return $ [ c | _ <- [1..number]]
    Right c  -> return [c]

decodeParser :: Parser String
decodeParser = do
  expandedChars <- many decodeCharacterRun
  return $ concat expandedChars

-- ENCODER

encode :: String -> String
encode decodedText = case (parser decodedText) of
  Success encoded -> encoded
  _               -> "can't encode given text"
  where parser = parseString encodeParser mempty

encodeCharacterRun :: Parser String
encodeCharacterRun = do
  c         <- anyChar
  remaining <- many $ char c
  let run = [c] ++ remaining
  return $ case run of
    "" -> ""
    [c] -> [c]
    (x:xs) -> (show $ length run) ++ [c]

encodeParser :: Parser String
encodeParser = do
  encoded <- many encodeCharacterRun
  return $ concat encoded
