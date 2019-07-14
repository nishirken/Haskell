{-# LANGUAGE OverloadedStrings #-}

module PhoneParse where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (isDigit)
import Data.Void (Void)
import Data.Text (Text)

data RussianPhone = RussianPhone
  { _cityCode :: Int
  , _lineNumber :: Int
  } deriving (Eq, Show)

type Parser = Parsec Void Text

phoneParser :: Parser RussianPhone
phoneParser = do
  char '8'
  optional $ try . some (char '(') <|> try . some (char '-') <|> some spaceChar
  areaCode <- areaCodeParser
  optional $ try .some (char ')') <|> try . some (char '-') <|> some spaceChar
  lineCode <- try fiveLineCodeParser <|> try sixLineCodeParser <|> sevenLineCodeParser
  pure $ RussianPhone areaCode lineCode

areaCodeParser :: Parser Int
areaCodeParser = do
  takeP Nothing 3
  (read :: String -> Int) <$> some digitChar

fiveLineCodeParser :: Parser Int
fiveLineCodeParser = do
  optional $ try . some (char '-') <|> some spaceChar
  
  
sixLineCodeParser :: Parser Int
sixLineCodeParser = spaceChar

sevenLineCodeParser :: Parser Int
sevenLineCodeParser = do
  optional $ try () <|> spaceChar
  spaceChar
