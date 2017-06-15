{-# LANGUAGE TemplateHaskell #-}
import Analysis
import Algebra
import Logic
import Test.QuickCheck
import Test.QuickCheck.All

prop_ackermann :: Int -> Bool
prop_ackermann n =
    Algebra.ack 0 n == n+ 1

prop_lawOfCos :: Float ->Float -> Property
prop_lawOfCos a b = a>0 && b>0 ==>
    Algebra.lawOfCosines a b 90 == sqrt( a^2 + b^2)

prop_deMorgan :: Int ->Int -> Bool
prop_deMorgan a b =
    Logic.deMorgan a b == 1

prop_const_derive_of_linear :: Float -> Float -> Float -> Bool
prop_const_derive_of_linear h x1 x2 =
    round (Analysis.derive (\x -> 2 *x) h x1) == round (Analysis.derive (\x -> 2 *x) h x2)

prop_increasing_deriveCubic :: Float -> Float -> Float -> Property
prop_increasing_deriveCubic h x1 x2 = x1 > x2 ==>
    round (Analysis.deriveCubic h x1) > round (Analysis.deriveCubic h x2)

prop_increasing_deriveSqr :: Float -> Float -> Float -> Property
prop_increasing_deriveSqr h x1 x2 = x1 > x2  ==>
     (Analysis.deriveSqr h x1) > (Analysis.deriveSqr h x2)

prop_deriveSqrt :: Float -> Float -> Property
prop_deriveSqrt h x = x /= 0 && h /= 0 ==>
      round (Analysis.deriveSqrt h x)  == round (1/ (2 * sqrt x))


return []
runTests = $quickCheckAll
main = runTests
