{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parse where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text.IO as TextIO
import Text.RawString.QQ

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
