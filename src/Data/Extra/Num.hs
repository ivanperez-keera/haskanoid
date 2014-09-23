module Data.Extra.Num where

ensurePos :: (Eq a, Num a) => a -> a
ensurePos e = if signum e == (-1) then negate e else e

ensureNeg :: (Eq a, Num a) => a -> a
ensureNeg e = if signum e == 1 then negate e else e
