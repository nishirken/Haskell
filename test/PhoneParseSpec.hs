{-# LANGUAGE OverloadedStrings #-}

module PhoneParseSpec where

import Text.Megaparsec (parse)
import Test.Hspec (describe, context, it, Spec, shouldBe)
import PhoneParse

phoneParseSpec :: Spec
phoneParseSpec = describe "Phone parsing" $ do
  let
    sevenLengthPhone = RussianPhone 343 2554413
    sixLengthPhone = RussianPhone 343 254413
    fiveLengthPhone = RussianPhone 343 24413

  context "all -" $ do
    it "7 length" $ parse phoneParser "" "8-343-255-44-13" `shouldBe` Right sevenLengthPhone
    it "6 length" $ parse phoneParser "" "8-343-25-44-13" `shouldBe` Right sixLengthPhone
    it "5 length" $ parse phoneParser "" "8-343-2-44-13" `shouldBe` Right fiveLengthPhone

  context "area code in () with -" $ do
    it "7 length" $ parse phoneParser "" "8-(343)-255-44-13" `shouldBe` Right sevenLengthPhone
    it "6 length" $ parse phoneParser "" "8-(343)-25-44-13" `shouldBe` Right sixLengthPhone
    it "5 length" $ parse phoneParser "" "8-(343)-2-44-13" `shouldBe` Right fiveLengthPhone

  context "area code in () with spaces" $ do
    it "7 length" $ parse phoneParser "" "8 (343) 255 44 13" `shouldBe` Right sevenLengthPhone
    it "6 length" $ parse phoneParser "" "8 (343) 25 44 13" `shouldBe` Right sixLengthPhone
    it "5 length" $ parse phoneParser "" "8 (343) 2 44 13" `shouldBe` Right fiveLengthPhone
  
  context "all spaces" $ do
    it "7 length" $ parse phoneParser "" "8 343 255 44 13" `shouldBe` Right sevenLengthPhone
    it "6 length" $ parse phoneParser "" "8 343 25 44 13" `shouldBe` Right sixLengthPhone
    it "5 length" $ parse phoneParser "" "8 343 2 44 13" `shouldBe` Right fiveLengthPhone

  context "line number joined" $ do
      it "7 length with -" $ parse phoneParser "" "8-343-2554413" `shouldBe` Right sevenLengthPhone
      it "7 length with spaces" $ parse phoneParser "" "8 343 2554413" `shouldBe` Right sevenLengthPhone
      it "7 length with area code in () and -" $
        parse phoneParser "" "8-(343)-2554413" `shouldBe` Right sevenLengthPhone
      it "7 length with area code in () and spaces" $
        parse phoneParser "" "8 (343) 2554413" `shouldBe` Right sevenLengthPhone
      it "6 length with -" $ parse phoneParser "" "8-343-254413" `shouldBe` Right sixLengthPhone
      it "6 length with spaces" $ parse phoneParser "" "8 343 254413" `shouldBe` Right sixLengthPhone
      it "6 length with area code in () and -" $
        parse phoneParser "" "8-(343)-254413" `shouldBe` Right sixLengthPhone
      it "6 length with area code in () and spaces" $
        parse phoneParser "" "8 (343) 254413" `shouldBe` Right sixLengthPhone
      it "5 length with -" $ parse phoneParser "" "8-343-24413" `shouldBe` Right fiveLengthPhone
      it "5 length with spaces" $ parse phoneParser "" "8 343 24413" `shouldBe` Right fiveLengthPhone
      it "5 length with area code in () and -" $
        parse phoneParser "" "8-(343)-24413" `shouldBe` Right fiveLengthPhone
      it "5 length with area code in () and spaces" $
        parse phoneParser "" "8 (343) 24413" `shouldBe` Right fiveLengthPhone

  context "all joined" $ do
      it "7 length" $ parse phoneParser "" "83432554413" `shouldBe` Right sevenLengthPhone
      it "7 length with area code in ()" $ parse phoneParser "" "8(343)2554413" `shouldBe` Right sevenLengthPhone
      it "6 length" $ parse phoneParser "" "8343254413" `shouldBe` Right sixLengthPhone
      it "6 length with area code in ()" $ parse phoneParser "" "8(343)254413" `shouldBe` Right sixLengthPhone
      it "5 length" $ parse phoneParser "" "834324413" `shouldBe` Right fiveLengthPhone
      it "5 length with area code in ()" $ parse phoneParser "" "8(343)24413" `shouldBe` Right fiveLengthPhone

  context "messy" $ do
    it "first" $ parse phoneParser "" "8 -343   2554413" `shouldBe` Right sevenLengthPhone
    it "second" $ parse phoneParser "" "8(343)-255 44   -13" `shouldBe` Right sevenLengthPhone
    it "third" $ parse phoneParser "" "8-343- 5 4413" `shouldBe` Right fiveLengthPhone
    it "fourth" $ parse phoneParser "" "8343 5 4413" `shouldBe` Right fiveLengthPhone
