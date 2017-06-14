module LogicSpec where

import Test.Hspec
import Logic

spec :: Spec
spec = do
  describe "Logic.xor" $
    it "returns the xor for two boolean values in 1/0 notation" $
      xor 0 1 `shouldBe` 1
      
  describe "Logic.deMorgan" $
    it "checks if deMorgan laws is correctly defined (1)" $
      deMorgan 1 1 `shouldBe` 1
  describe "Logic.deMorgan" $
    it "checks if deMorgan laws is correctly defined (2)" $
      deMorgan 1 0 `shouldBe` 1
  describe "Logic.deMorgan" $
    it "checks if deMorgan laws is correctly defined (3)" $
      deMorgan 0 1 `shouldBe` 1
  describe "Logic.deMorgan" $
    it "checks if deMorgan laws is correctly defined (4)" $
      deMorgan 0 0 `shouldBe` 1
