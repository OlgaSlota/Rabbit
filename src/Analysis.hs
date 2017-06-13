module Analysis (derive, macLaurinEx) where

derive :: (Fractional a) => a -> (a -> a) -> (a -> a)
derive h f x = (f (x+h) - f x) / h

macLaurinEx :: Int -> Float -> Float
macLaurinEx n x = sum $ take n (expt x)
    where expt x = [x**n / product [1..n] | n <- [0..]]
