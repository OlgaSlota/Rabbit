module Logic (xor, and', or', deMorgan) where

xor :: Int -> Int -> Int
xor 1 x = not' x
xor 0 x =  x

not' :: Int -> Int
not' 0 = 1
not' 1 = 0

and' :: Int -> Int -> Int
and' 1 1 = 1
and' _ _ = 0

or' :: Int -> Int -> Int
or' 0 0 = 0
or' _ _ = 1

deMorgan :: Int -> Int -> Int
deMorgan p q = case not' (and' p q) == or' (not' p) (not' q) of
        True -> 1
        _ -> 0
