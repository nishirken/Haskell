module Semver where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Maybe (fromMaybe)
import Data.Foldable (fold)

data Semver = Semver
  { _version :: Version
  , _preRelease :: [AlphaNum]
  , _metadata :: [AlphaNum]
  } deriving (Show)

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

instance Eq Semver where
  (Semver version release _) == (Semver version' release' _) =
    version == version' && release == release'

instance Ord Version where
  (Version major minor patch) `compare` (Version major' minor' patch') =
    if major < major'
      then LT
      else if minor < minor'
        then LT
        else patch `compare` patch'

instance Ord Semver where
  (Semver version release _) `compare` (Semver version' release' _) =
    if version == version' then release `compareRelease` release' else version `compare` version'
      where
        compareRelease :: [AlphaNum] -> [AlphaNum] -> Ordering
        compareRelease _ [] = LT
        compareRelease [] _ = GT
        compareRelease r r' = if comparedZipped == EQ
          then length r `compare` length r'
          else comparedZipped
            where
              compareAlphaNum :: (AlphaNum, AlphaNum) -> Ordering
              compareAlphaNum ((NOSS x), (NOSS y)) = x `compare` y
              compareAlphaNum ((NOSI x), (NOSS y)) = LT
              compareAlphaNum ((NOSS x), (NOSI y)) = GT
              compareAlphaNum ((NOSI x), (NOSI y)) = x `compare` y
              comparedZipped :: Ordering
              comparedZipped = fold $ map compareAlphaNum $ zip r r'
