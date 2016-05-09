-- | A trivial collision subsystem.
--
-- Based on the physics module, it determines the side of collision
-- between shapes.
module Physics.TwoDimensions.Collisions where

import FRP.Yampa.VectorSpace as Yampa
import Physics.TwoDimensions.Physics

-- * Collision sides

-- | Collision side of a rectangle
data Side = TopSide | BottomSide | LeftSide | RightSide
  deriving (Eq,Show)

-- | Opposite side during a collision.
--
-- If A collides with B, the collision sides on
-- A and B are always opposite.
oppositeSide :: Side -> Side
oppositeSide TopSide    = BottomSide
oppositeSide BottomSide = TopSide
oppositeSide LeftSide   = RightSide
oppositeSide RightSide  = LeftSide

-- | Calculates the collision side of a shape
-- that collides against another.
--
-- PRE: the shapes do collide. Use 'overlapShape' to check.
shapeCollisionSide :: Shape -> Shape -> Side
shapeCollisionSide (Rectangle p1 s1) (Rectangle p2 s2)
   | wy > hx && wy > -hx = TopSide
   | wy > hx             = LeftSide
   | wy > -hx            = RightSide
   | otherwise           = BottomSide
  where (dx,dy) = (p1 ^+^ (0.5 *^ s1)) ^-^ (p2 ^+^ (0.5 *^ s2)) -- p1 ^-^ p2
        (w,h)   = 0.5 *^ (s1 ^+^ s2)
        wy      = w * dy
        hx      = h * dx
