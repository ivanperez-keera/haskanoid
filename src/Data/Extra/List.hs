-- | Auxiliary functions related to Data.List.
module Data.Extra.List where

mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p = map f . filter p 
