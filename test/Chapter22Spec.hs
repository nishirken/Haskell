module Chapter22Spec where

import Test.Hspec (describe, it, Spec, shouldBe)
import Chapter22

chapter22Spec :: Spec
chapter22Spec = describe "Chapter22Spec" $ do
  it "Functor" $ do
    composed "Julie" `shouldBe` "EILUJ"
    fmapped "Chris" `shouldBe` "SIRHC"

  it "Applicative" $ do
    tupled "Julie" `shouldBe` ("JULIE", "eiluJ")
    tupled' "Julie" `shouldBe` ("eiluJ", "JULIE")

  it "Monad" $ do
    tupled'' "Julie" `shouldBe` ("JULIE", "eiluJ")
    tupled''' "Julie" `shouldBe` ("JULIE", "eiluJ")
