module ProcessCalculationSpec where

import Test.Hspec
import ProcessCalculation

spec :: Spec
spec = do
    describe "ProcessCalculation.processMsg" $
     it "returns the result of the function called" $
       processMsg "logic" ["xor", "1", "0"] `shouldBe` 1.0
