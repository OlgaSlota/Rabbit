module Logic (xor) where

xor :: Int -> Int -> Int
xor 1 x = not' x
xor 0 x =  x

not' :: Int -> Int
not' 0 = 1
not' 1 = 0