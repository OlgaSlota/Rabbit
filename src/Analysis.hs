module Analysis (deriveSqrt, deriveSqr, deriveCubic, deriveTan, derive, macLaurinEx) where

derive :: (Fractional a, Floating a) => (a -> a) -> a -> (a -> a)
derive f h x = (f (x+h) - f x) / h

deriveSqrt :: (Floating a) => a -> (a -> a)
deriveSqrt = derive sqrt

deriveSqr :: (Floating a) => a -> (a -> a)
deriveSqr = derive (^2)

deriveCubic :: (Floating a) => a -> (a -> a)
deriveCubic = derive (^3)

deriveTan :: (Floating a) => a -> (a -> a)
deriveTan = derive tan

macLaurinEx :: Int -> Float -> Float
macLaurinEx n x = sum $ take n (expt x)
    where expt x = [x**n / product [1..n] | n <- [0..]]
