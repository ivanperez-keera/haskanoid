-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Auxiliary functions related to the 'Ord' typeclass.
module Data.Extra.Ord where

inRange :: Ord a => (a, a) -> a -> a
inRange (mN, mX) x = min mX (max mN x)
