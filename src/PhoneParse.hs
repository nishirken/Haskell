module PhoneParse where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (isDigit)
import Data.Void (Void)
import Data.Text (Text)
import Control.Monad (replicateM)

data RussianPhone = RussianPhone
  { _cityCode :: Int
  , _lineNumber :: Int
  } deriving (Eq, Show)

type Parser = Parsec Void Text

delimiter :: Parser Char
delimiter = try (char '-') <|> spaceChar

phoneParser :: Parser RussianPhone
phoneParser = do
  char '8'
  areaCode <- (read :: String -> Int) <$> areaCodeParser
  lineCode <- try fiveLineCodeParser <|> try sixLineCodeParser <|> sevenLineCodeParser
  pure $ RussianPhone areaCode lineCode

areaCodeParser :: Parser String
areaCodeParser = leftSign *> count 3 digitChar <* rightSign
  where
    leftSign = skipMany $ try (char '(') <|> delimiter
    rightSign = skipMany $ try (char ')') <|> delimiter

digits :: Parser String
digits =
  skipMany delimiter *> some digitChar <* skipMany delimiter

concatDigitsChunks n = concat <$> replicateM n digits

fiveLineCodeParser :: Parser Int
fiveLineCodeParser = (read :: String -> Int) <$> (try oneTwoTwo <|> try threeTwo <|> five)
  where
    oneTwoTwo = concatDigitsChunks 3
    threeTwo = concatDigitsChunks 2
    five = digits

sixLineCodeParser :: Parser Int
sixLineCodeParser = (read :: String -> Int) <$> (try (concatDigitsChunks 3) <|> try (concatDigitsChunks 2) <|> six)
  where
    twoTwoTwo = concatDigitsChunks 3
    threeThree = concatDigitsChunks 2
    six = digits

sevenLineCodeParser :: Parser Int
sevenLineCodeParser = (read :: String -> Int) <$> (try oneTwoTwoTwo <|> try threeTwoTwo <|> seven)
  where
    oneTwoTwoTwo = concatDigitsChunks 4
    threeTwoTwo = concatDigitsChunks 3
    seven = digits
