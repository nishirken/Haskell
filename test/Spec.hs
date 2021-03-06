import Test.Hspec
import Chapter12 (notThe, replaceThe)
import Chapter11.HuttonsRazor (Expr (Lit, Add), eval, printExpr)

import MonoidSpec (monoidSpec)
import FunctorSpec (functorSpec)
import ApplicativeSpec (applicativeSpec)
import MonadSpec (monadSpec)
import FoldableSpec (foldableSpec)
import TraversableSpec (traversableSpec)
import ParseSpec (parseSpec)
import SemverSpec (semverSpec)
import PhoneParseSpec (phoneParseSpec)

main :: IO ()
main = hspec $ do
    describe "HuttonsRazor" $ do
        context "eval" $
            it "works" $
                eval (Add (Lit 1) (Lit 9001)) `shouldBe` 9002
        context "print Expr" $
            it "works" $
                printExpr (Add (Lit 1) (Add (Add (Lit 9001) (Lit 1)) (Lit 20001))) `shouldBe` "1 + 9001 + 1 + 20001"
    describe "Chapter 12" $ do
        context "notThe" $ do
            it "Just x, if x not 'the'" $
                notThe "blahblahthe" `shouldBe` Just "blahblahthe"

            it "Nothing, if x 'the'" $
                notThe "the" `shouldBe` Nothing

        context "replaceThe" $
            it "replace the with a" $
                replaceThe "the cow loves us" `shouldBe` "a cow loves us"

    monoidSpec
    functorSpec
    applicativeSpec
    monadSpec
    foldableSpec
    traversableSpec
    parseSpec
    semverSpec
    phoneParseSpec
