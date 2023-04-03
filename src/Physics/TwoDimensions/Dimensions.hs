-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Physical dimensions used all over the game. They are just type synonyms, but
-- it's best to use meaningful names to make our type signatures more
-- meaningful.
module Physics.TwoDimensions.Dimensions where

type Size2D = (Double, Double)

type Pos2D  = (Double, Double)

type Vel2D  = (Double, Double)

type Acc2D  = (Double, Double)
