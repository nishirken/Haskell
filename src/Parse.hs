{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parse where

import Control.Applicative
import Text.Trifecta
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text.IO as TextIO
import Text.RawString.QQ

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

type Name = String
type Value = String
type Assignments = M.Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  val <- some (noneOf "\n")
  skipEOL
  pure (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
  "; blah\n; woot\n  \n;hah"

skipComments :: Parser ()
skipComments =
  skipMany $ do
    char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

sectionEx :: ByteString
sectionEx =
  "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
  ; ignore me
  [states]
  Chris=Texas
  |]

sectionEx'' :: ByteString
sectionEx'' = [r|
  ; comment
  [section] host=wikipedia.org alias=claw
  [whatisit] red=intoothandclaw
  |]
