{-# LANGUAGE OverloadedStrings #-}

module SemverSpec where

import Semver
import Text.Megaparsec (parse)
import Test.Hspec (context, describe, it, Spec, shouldBe)

semverSpec :: Spec
semverSpec = describe "Semver parsing" $ do
  it "integer parsing" $
    parse parseInteger "" "1223" `shouldBe` Right 1223

  context "alphaNumParser" $ do
    it "text" $
      parse alphaNumParser "" "alpha" `shouldBe` (Right $ NOSS "alpha")
    it "integer" $
      parse alphaNumParser "" "2244" `shouldBe` (Right $ NOSI 2244)

  context "release or meta" $ do
    it "first" $ do
      let expect = [NOSS "alpha"]
      parse releaseOrMetaParser "" "alpha" `shouldBe` Right expect
    it "second" $ do
      let expect = [NOSI 1, NOSS "alpha", NOSI 0, NOSI 5]
      parse releaseOrMetaParser "" "1-alpha-0.5" `shouldBe` Right expect

  context "only version" $ do
    it "first" $ do
      let expect = Semver (Version 22 4 5) [] []
      parse semverParser "" "22.4.5" `shouldBe` Right expect
    it "second" $ do
      let expect = Semver (Version 0 0 11) [] []
      parse semverParser "" "0.0.11" `shouldBe` Right expect
    it "third" $ do
      let expect = Semver (Version 2 1 1) [] []
      parse semverParser "" "2.1.1" `shouldBe` Right expect

  context "with release" $ do
    it "first" $ do
      let expect = Semver (Version 1 0 0) [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
      parse semverParser "" "1.0.0-x.7.z.92" `shouldBe` Right expect
    it "second" $ do
      let expect = Semver (Version 0 0 0) [NOSS "alpha", NOSI 0, NOSI 5] []
      parse semverParser "" "0.0.0-alpha-0.5" `shouldBe` Right expect

  context "with metainformation" $ do
    it "without release" $ do
      let expect = Semver (Version 1 11 7) [] [NOSI 20130313144700]
      parse semverParser "" "1.11.7+20130313144700" `shouldBe` Right expect

    it "with release" $ do
      let expect = Semver (Version 1 0 0) [NOSS "beta"] [NOSS "exp", NOSS "sha", NOSS "5114f85", NOSI 0, NOSI 1]
      parse semverParser "" "1.0.0-beta+exp.sha.5114f85-0.1" `shouldBe` Right expect
