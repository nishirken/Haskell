module Semver where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Maybe (fromMaybe)

data Semver = Semver
  { _version :: Version
  , _preRelease :: [AlphaNum]
  , _metadata :: [AlphaNum]
  } deriving (Eq, Show)

data Version = Version
  { _major :: Integer
  , _minor :: Integer
  , _patch :: Integer
  } deriving (Eq, Show)

data AlphaNum = NOSS Text | NOSI Integer
  deriving (Eq, Show)

type Parser = Parsec Void Text

semverParser :: Parser Semver
semverParser = do
  version <- versionParser
  release <- optional . try $ do
    char '-'
    releaseOrMetaParser
  meta <- optional . try $ do
    char '+'
    releaseOrMetaParser
  eof
  pure $ Semver version (fromMaybe [] release) (fromMaybe [] meta)

parseInteger :: Parser Integer
parseInteger = do
  xs <- (read :: String -> Integer) <$> some digitChar
  notFollowedBy letterChar
  pure xs

versionParser :: Parser Version
versionParser = do
  major <- parseInteger
  char '.'
  minor <- parseInteger
  char '.'
  patch <- parseInteger
  pure $ Version major minor patch

alphaNumParser :: Parser AlphaNum
alphaNumParser = try (NOSI <$> parseInteger) <|> NOSS . pack <$> some alphaNumChar

releaseOrMetaParser :: Parser [AlphaNum]
releaseOrMetaParser = some $ do
  xs <- alphaNumParser
  optional $ try (char '.') <|> char '-'
  pure xs
