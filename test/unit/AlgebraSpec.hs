module AlgebraSpec where

import Test.Hspec
import Algebra

spec :: Spec
spec = do
  describe "Algebra.ack" $
    it "returns the ackermann function value of two numbers" $
      ack 0 2`shouldBe` 3

  describe "Algebra.qsort" $
    it "sorts the given list" $
      qsort [5,0,3,1,5,9,2] `shouldBe` [0,1,2,3,5,5,9]
      
  describe "Algebra.lawOfCosines" $
    it "counts the 3rd side of triangle" $
      lawOfCosines 3 4 90 `shouldBe` 5.0
