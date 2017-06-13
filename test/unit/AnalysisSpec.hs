module AnalysisSpec where

import Test.Hspec
import Test.QuickCheck

import Analysis
import Text.Printf

spec :: Spec
spec = do
  describe "Analysis.derive" $
    it "returns the derivative for delta, function and value" $
      round (derive 0.00001 (\x -> 2 *x) 5 ) `shouldBe` 2

  describe "Analysis.derive" $
    it "is constant when function linear" $
      round (derive 0.00001 (\x -> 2 *x) 5 ) == round (derive 0.00001 (\x -> 2 *x) 2 )

  describe "Analysis.macLaurinEx" $
    it "returns sum of maclaurin e^x" $
      printf "%.2f" (macLaurinEx 5 10) `shouldBe` "644.33"
