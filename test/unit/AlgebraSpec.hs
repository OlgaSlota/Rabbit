module AlgebraSpec where

import Test.Hspec
import Algebra

spec :: Spec
spec = do
  describe "Algebra.ack" $ do
    it "returns the ackermann function value of two numbers" $ do
      ack 0 2`shouldBe` 3