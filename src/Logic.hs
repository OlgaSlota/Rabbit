module Logic where

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x
