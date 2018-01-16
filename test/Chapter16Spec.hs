module Chapter16Spec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Chapter16 (Identity)

chapter16spec :: SpecWith ()
chapter16spec =
    describe "Chapter16 Functor" $ do
        context "Functor instances" $ do
            it "Identity compose works" $ property $ do
                functorCompose' :: IntFC

