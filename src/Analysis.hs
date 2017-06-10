module Analysis where

derive :: (Fractional a) => a -> (a -> a) -> (a -> a)
derive h f x = (f (x+h) - f x) / h

multiply :: (Num a) => a -> a -> a
multiply x y = x * y

