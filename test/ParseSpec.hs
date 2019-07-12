{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParseSpec where

import Test.Hspec (context, describe, it, Spec, shouldBe)
import Parse
import Data.ByteString (ByteString)
import Text.Trifecta (parseByteString, parseTest, Result (..), some)
import Text.RawString.QQ
import qualified Data.Map as M

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

  it "Section" $ do
    let
      sectionEx :: ByteString
      sectionEx = "; ignore me  \n  [states]\n  [states22.0]   \n  Chris=Texas  \n\n"
      expect :: IniSection
      expect = IniSection [Header "states", Header "states22.0"] $ M.fromList [("Chris", "Texas")]
    parseByteString parseSection mempty sectionEx `shouldBe` Success expect

  context "Entire test" $ do
    it "First" $ do
      let
        sectionEx' :: ByteString
        sectionEx' = [r|


          ; ignore me
          [states]
          Chris=Texas
          |]
        expect :: IniConfig
        expect = [IniSection [Header "states"] $ M.fromList [("Chris", "Texas")]]
      parseByteString parseIni mempty sectionEx' `shouldBe` Success expect

    it "Second" $ do
      let
        sectionEx'' :: ByteString
        sectionEx'' = [r|

          ; comment
          [section]
          host=wikipedia.org
          alias=claw


          # unix comment
          [whatisit]
          [subtitle]
          red=intoothandclaw
          blue=white

          |]
        expect :: IniConfig
        expect =
          [ IniSection [Header "section"] $
              M.fromList [("host", "wikipedia.org"), ("alias", "claw")]
          , IniSection [Header "whatisit", Header "subtitle"] $
              M.fromList [("red", "intoothandclaw"), ("blue", "white")]
          ]
      parseByteString parseIni mempty sectionEx'' `shouldBe` Success expect
