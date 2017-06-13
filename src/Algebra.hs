module Algebra (ack, qsort, lawOfCosines) where

ack :: Int -> Int -> Int
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++
               [x] ++
               qsort (filter (>= x) xs)

lawOfCosines :: Float -> Float -> Float -> Float
lawOfCosines a b cos_ = fromInteger $ round $ fun * (10^2) / (10.0^^2)
    where fun = sqrt(a**2 + b**2 - 2*a*b*cos(cos_/180*pi))
