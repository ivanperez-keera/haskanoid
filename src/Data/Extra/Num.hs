-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Auxiliary functions related to the 'Num' typeclass.
module Data.Extra.Num where

ensurePos :: (Eq a, Num a) => a -> a
ensurePos e = if signum e == (-1) then negate e else e

ensureNeg :: (Eq a, Num a) => a -> a
ensureNeg e = if signum e == 1 then negate e else e
