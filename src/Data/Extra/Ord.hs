module Data.Extra.Ord where

inRange :: Ord a => (a,a) -> a -> a
inRange (mN, mX) x = min mX (max mN x)

