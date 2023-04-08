-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Auxiliary functions related to Data.VectorSpace.
module Data.Extra.VectorSpace where

-- External imports
import Data.VectorSpace

limitNorm :: (Ord s, VectorSpace v s) => v -> s -> v
limitNorm v mn = if norm v > mn then mn *^ normalize v else v
