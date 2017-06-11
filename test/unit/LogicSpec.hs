module LogicSpec where

import Test.Hspec
import Logic

spec :: Spec
spec = do
  describe "Logic.xor" $ do
    it "returns the xor for two boolean values in 1/0 notation" $ do
      xor 0 1 `shouldBe` 1