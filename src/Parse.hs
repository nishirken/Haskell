{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative
import Text.Trifecta
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text.IO as TextIO

data IniSection = IniSection
  { _headers :: [Header]
  , _assignments :: Assignments
  } deriving (Eq, Show)

type IniConfig = [IniSection]

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = do
  header <- parseBracketPair (Header <$> some (alphaNum <|> char '.'))
  skipWhitespace
  pure header

type Name = String
type Value = String
type Assignments = M.Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  val <- some (noneOf "\t \n")
  skipWhitespace
  pure (name, val)

skipComments :: Parser ()
skipComments =
  skipMany $ do
    char ';' <|> char '#'
    skipMany (noneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char '\t' <|> char ' ' <|> char '\n')

parseSection :: Parser IniSection
parseSection = do
  skipWhitespace
  skipComments
  skipWhitespace
  headers <- some parseHeader
  assignments <- some parseAssignment
  pure $ IniSection headers (M.fromList assignments)

parseIni :: Parser IniConfig
parseIni = some parseSection
