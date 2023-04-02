-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Auxiliary functions related to Data.List.
module Data.Extra.List where

mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p = map f . filter p 
