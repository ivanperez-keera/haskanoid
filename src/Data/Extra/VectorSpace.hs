module Data.Extra.VectorSpace where

import FRP.Yampa.VectorSpace

limitNorm :: (Ord s, VectorSpace v s) => v -> s -> v
limitNorm v mn = if norm v > mn then mn *^ normalize v else v
