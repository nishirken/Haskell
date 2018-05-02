module TimeMaskSpec (timeMaskSpec) where

import Test.Hspec
import Data.Monoid
import TimeMask (prepairInputValue)

timeMaskSpec :: SpecWith ()
timeMaskSpec =
	describe "Time mask" $
		it "Splits in hours and minutes" $ prepairInputValue "22:11" `shouldBe` [22, 11]
