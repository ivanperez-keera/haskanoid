-- | Auxiliary functions related to Data.VectorSpace.
module Data.Extra.VectorSpace where

import Data.VectorSpace

limitNorm :: (Ord s, VectorSpace v s) => v -> s -> v
limitNorm v mn = if norm v > mn then mn *^ normalize v else v
