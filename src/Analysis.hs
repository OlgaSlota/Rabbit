module Analysis (derive) where

derive :: (Fractional a) => a -> (a -> a) -> (a -> a)
derive h f x = (f (x+h) - f x) / h
