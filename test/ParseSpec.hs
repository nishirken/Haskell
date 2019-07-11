{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import Test.Hspec (context, describe, it, Spec, shouldBe)
import Parse
import Data.ByteString (ByteString)
import Text.Trifecta (parseByteString, Result (..), some)

instance Eq a => Eq (Result a) where
  (Success x) == (Success y) = x == y
  _ == _ = False

parseSpec :: Spec
parseSpec = describe "Parser" $ do
  it "Header" $
    parseByteString parseHeader mempty ("[blah]" :: ByteString) `shouldBe` (Success $ Header "blah")

  it "Assignment" $
    parseByteString (some parseAssignment) mempty ("key=value\n\n\ntest=data" :: ByteString) `shouldBe`
      (Success $ [("key", "value"), ("test", "data")])
