-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- A very simple physics subsystem. It currently detects shape overlaps only,
-- the actual physics movement is carried out in Yampa itself, as it is very
-- simple using integrals and derivatives.
module Physics.TwoDimensions.Physics where

-- External imports
import Data.VectorSpace

-- Internal imports
import Physics.TwoDimensions.Dimensions

data Shape = Rectangle Pos2D Size2D   -- A corner and the whole size
             -- Circle    Pos2D Float -- Position and radius -- NOT FOR NOW
             -- SemiPlane Pos2D Float -- Position, angle of plane normal NFN

-- | Detects if two shapes overlap.
--
-- Rectangles: overlap if projections on both axis overlap, which happens if x
-- distance between centers is less than the sum of half the widths, and the
-- analogous for y and the heights.

overlapShape :: Shape -> Shape -> Bool
overlapShape (Rectangle p1 s1) (Rectangle p2 s2) = abs dx <= w && abs dy <= h
  where
    (dx, dy) = (p1 ^+^ (0.5 *^ s1)) ^-^ (p2 ^+^ (0.5 *^ s2))
    (w, h)   = 0.5 *^ (s1 ^+^ s2)
