module AnalysisSpec where

import Test.Hspec
import Test.QuickCheck

import Analysis

spec :: Spec
spec = do
  describe "Analysis.derive" $ do
    it "returns the derivative for delta, function and value" $ do
      round (derive 0.00001 (\x -> 2 *x) 5 ) `shouldBe` 2

  describe "Analysis.derive" $ do
    it "is constant when function linear" $ do
      round (derive 0.00001 (\x -> 2 *x) 5 ) == round (derive 0.00001 (\x -> 2 *x) 2 )