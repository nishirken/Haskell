import Test.Hspec
import Chapter12

main :: IO ()
main = hspec $ do
--   describe "HuttonsRazor" $ do
--     it "eval" $ do
--       eval (Add (Lit 1) (Lit 9001)) `shouldBe` 9002
    describe "Chapter 12" $ do
        context "notThe" $ do
            it "Just x, if x not 'the'" $ do
                notThe "blahblahthe" `shouldBe` Just "blahblahthe"

            it "Nothing, if x 'the'" $ do
                notThe "the" `shouldBe` Nothing

        context "replaceThe" $ do
            it "replace the with a" $ do
                replaceThe "the cow loves us" `shouldBe` "a cow loves us"
