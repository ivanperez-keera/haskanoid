-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Level definition
--
-- This module includes the definition of the levels.
--
-- Each level has:
--
--   - Logical properties (defined in 'blockDescS').
--
--   - Multimedia properties (a background and audio file).
--
-- Together they form 'levels'.
module Levels where

-- External imports
import Control.Arrow (first, (***))
import Data.List     (nub)

-- Internal imports
import Constants
import Physics.TwoDimensions.Dimensions
import Resources

-- * Levels
-- ** Level specification
data LevelSpec = LevelSpec
  { blockCfgs  :: [(Pos2D, Int)] -- ^ Block positions and block lives
  , levelBg    :: ImageResource  -- ^ Background image
  , levelMusic :: MusicResource  -- ^ Background music
  }

-- | Number of levels. Change this in the code to finish in a different level.
numLevels :: Int
numLevels = length levels

-- * Concrete levels
levels :: [LevelSpec]
levels = [ -- Level 0
           LevelSpec { blockCfgs  = blockDescS 0
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level0.mp3"
                     }
         ,
           -- Level 1
           LevelSpec { blockCfgs  = blockDescS 1
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 2
           LevelSpec { blockCfgs  = blockDescS 2
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 3
           LevelSpec { blockCfgs  = blockDescS 3
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level0.mp3"
                     }
         ,
           -- Level 4
           LevelSpec { blockCfgs  = blockDescS 4
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 5
           LevelSpec { blockCfgs  = blockDescS 5
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 6
           LevelSpec { blockCfgs  = blockDescS 6
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level0.mp3"
                     }
         ,
           -- Level 7
           LevelSpec { blockCfgs  = blockDescS 7
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 8
           LevelSpec { blockCfgs  = blockDescS 8
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 9
           LevelSpec { blockCfgs  = blockDescS 9
                     , levelBg    = Resource "data/level0.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 10
           LevelSpec { blockCfgs  = blockDescS 10
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 11
           LevelSpec { blockCfgs  = blockDescS 11
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 12
           LevelSpec { blockCfgs  = blockDescS 12
                     , levelBg    = Resource "data/level0.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 13
           LevelSpec { blockCfgs  = blockDescS 13
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 14
           LevelSpec { blockCfgs  = blockDescS 14
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 15
           LevelSpec { blockCfgs  = blockDescS 15
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level0.mp3"
                     }
         ,
           -- Level 16
           LevelSpec { blockCfgs  = blockDescS 16
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 17
           LevelSpec { blockCfgs  = blockDescS 17
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ]

-- | Level block specification (positions, lives of block)

-- Level 0
--   %%%%%%%%
-- % XXXXXXXX
-- % XXXXXXXX
-- % XXXXXXXX
-- % XXXXXXXX
blockDescS :: Int -> [(Pos2D, Int)]
blockDescS 0 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                ]

    blockRows :: Num a => a
    blockRows = 4

-- Level 1
--   %%%%%%%%
-- %    XXXX
-- %   XXXXX
-- %  XXXXXX
-- % XXXXXX
-- % XXXXX
-- % XXXX
--
blockDescS 1 = map (first adjustPos) allBlocks
  where
    allBlocks :: (Enum a, Num a, Eq a, Ord a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                , (x + y > 2) && (x + y < 10)
                ]

    blockRows :: Num a => a
    blockRows = 6

-- Level 2
blockDescS 2  = map (first adjustPos) allBlocks
  where
    allBlocks :: (Enum a, Num a, Eq a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                , x /= y && (blockColumns - 1 - x) /= y
                ]

    blockRows :: Num a => a
    blockRows = 4


-- Level 3
--   %%%%%%%%
-- %  X X X X
-- % X X X X
-- %  X X X X
-- % X X X X
blockDescS 3 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                , even x && odd y || odd x && even y
                ]

    blockRows :: Num a => a
    blockRows = 4

-- Level 4
--   %%%%%%%%
-- % XXXXXXXX
-- %
-- %  X X X X
-- %
-- % X X X X
-- %
-- % XXXXXXXX
blockDescS 4 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0, blockRows - 1]
                ]
             ++ [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [2], odd x
                ]
             ++ [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [4], even x
                ]

    blockRows :: Num a => a
    blockRows = 7

-- Level 5
--   %%%%%%%%
-- %    X
-- %   X X
-- %  X   X
-- % XXXXXXX
blockDescS 5 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = nub $
         [((3, 0), maxBlockLife), ((blockColumns - 4, 1), maxBlockLife)]
      ++ [((2, 1), maxBlockLife), ((blockColumns - 3, 1), maxBlockLife)]
      ++ [((1, 2), maxBlockLife), ((blockColumns - 2, 2), maxBlockLife)]
      ++ [((x, y), maxBlockLife) | x <- [0..blockColumns - 1], y <- [3]]

-- Level 6
--   %%%%%%%%
-- %  XXXXXX
-- % X      X
-- % X      X
-- % X      X
-- %  XXXXXX
blockDescS 6 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [1..blockColumns - 2]
                , y <- [0, blockRows - 1]
                ]
             ++ [ ((x, y), maxBlockLife)
                | x <- [0, blockColumns - 1]
                , y <- [1..blockRows - 2]
                ]

    blockRows :: Num a => a
    blockRows = 5

-- Level 7
--   %%%%%%%%
-- %  XXXXXX
-- % X      X
-- %    XX
-- %    XX
-- % X      X
-- %  XXXXXX
blockDescS 7 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [1..blockColumns - 2]
                , y <- [0, blockRows - 1]
                ]
             ++ [ ((x, y), maxBlockLife)
                | x <- [0, blockColumns - 1]
                ,  y <- [1, blockRows - 2]
                ]
             ++ [ ((x, y), maxBlockLife)
                | x <- [3, 4]
                , y <- [2..4]
                ]

    blockRows :: Num a => a
    blockRows = 7

-- Level 8
--   %%%%%%%%
-- % XX XXXXX
-- % XX XXXXX
-- %
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
-- % XX XXXXX
blockDescS 8 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                , x /= 2
                , y /= 2
                ]

    blockRows :: Num a => a
    blockRows = 9

-- Level 9
--   %%%%%%%%
-- %    X
-- %   XXX
-- %  X X X
-- % XXXXXXX
-- %  X X X
-- %   XXX
-- %    X
blockDescS 9 = map (first ((adjustHPos *** adjustVPos) . fI2)) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife) | x <- [3], y <- [0..6] ]
             ++ [ ((x, y), maxBlockLife) | x <- [0..6], y <- [3] ]
             ++ [ ((x, y), maxBlockLife) | x <- [2, 4], y <- [1, 5] ]
             ++ [ ((x, y), maxBlockLife) | x <- [1, 5], y <- [2, 4] ]

    adjustHPos :: Double -> Double
    adjustHPos = (leftMargin +) . ((blockWidth + blockSeparation) *)

    leftMargin :: Num a => a
    leftMargin =
        round' ((gameWidth - (blockWidth + blockSeparation) * 7) / 2)
      where
        round' = fromIntegral . floor

-- Level 10
--   %%%%%%%%
-- %  X X X
-- %  X X X
-- %  X X X
-- %  X X X
-- % XXXXXXX
-- %  X X X
-- %  X X X
-- %  X X X
-- %  X X X
blockDescS 10 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                , odd x
                ]
             ++ [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [midRow]
                , even x
                ]

    blockRows :: Num a => a
    blockRows = 9

    midRow :: Integral a => a
    midRow = blockRows `div` 2

-- Level 11
--   %%%%%%%%
-- % XX
-- % XXXX
-- % XXXXXX
-- % XXXXXXXX
-- % XXXXXX
-- % XXXX
-- % XX
blockDescS 11 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | y <- [0 .. blockRows - 1]
                , x <- [0 .. (blockColumns - 1) - (2 * abs (y - midRow))]
                ]

    blockRows :: Num a => a
    blockRows = 7

    midRow :: Integral a => a
    midRow = blockRows `div` 2

-- Level 12
--   %%%%%%%%
-- % XXXXXXX
-- %   X X
-- %   X X
-- %
-- %  X   X
-- % X     X

blockDescS 12 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0]
                ]
             ++ [ ((x, y), maxBlockLife)
                | x <- [2, blockColumns - 3]
                , y <- [1, 2]
                ]
             ++ [ ((1, 4), maxBlockLife)
                , ((blockColumns - 2, 4), maxBlockLife)
                , ((0, 5), maxBlockLife)
                , ((blockColumns - 1, 5), maxBlockLife)
                ]

    blockRows :: Num a => a
    blockRows = 9

-- Level 13
-- X == maxBlockLife
-- O == maxBlockLife - minBlockLife
--   %%%%%%%%
-- % XOXOXOXO
-- % OXOXOXOX
-- % XOXOXOXO
-- % OXOXOXOX
blockDescS 13 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), blockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                , let blockLife =
                        if even (x + y)
                          then maxBlockLife
                          else maxBlockLife - 1
                ]

    blockRows :: Num a => a
    blockRows = 4

-- Level 14
-- X == maxBlockLife
-- O == maxBlockLife - minBlockLife
-- Y == minBlockLife
--   %%%%%%%%
-- % YYYYYYYY
-- % YXXXXXXY
-- % YXOOOOXY
-- % YXXXXXXY
-- % YYYYYYYY
blockDescS 14 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), minBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..blockRows - 1]
                , (x == 0)
                  || (y == 0)
                  || (x == blockColumns - 1)
                  || (y == blockRows - 1)]

             ++ [ ((x, y), maxBlockLife - 1)
                | x <- [1..blockColumns - 2]
                , y <- [1..blockRows - 2]
                , (x == 1)
                  || (y == 1)
                  || (x == blockColumns - 2)
                  || (y == blockRows - 2)]

             ++ [ ((x, y), maxBlockLife)
                | x <- [2..blockColumns - 3]
                , y <- [2..blockRows - 3]
                , (x == 2)
                  || (y == 2)
                  || (x == blockColumns - 3)
                  || (y == blockRows - 3)]

    blockRows :: Num a => a
    blockRows = 5

-- Level 15
-- maxBlockLife == X
-- minBlockLife == O
-- blockLife == T
--   %%%%%%%%
-- %  X X X
-- %  X X X
-- %  X X X
-- %  X X X
-- % OXOXOXO
-- % T T T
-- % T T T
-- % T T T
-- % T T T
blockDescS 15 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0..midRow]
                , odd x
                ]
             ++ [ ((x, y), minBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [midRow]
                , even x
                ]
             ++ [ ((x, y), maxBlockLife - 1)
                | x <- [0..blockColumns - 1]
                , y <- [midRow + 1..blockColumns - 1]
                , even x
                ]

    blockRows :: Num a => a
    blockRows = 9

    midRow :: Integral a => a
    midRow = blockRows `div` 2

-- Level 16
-- maxBlockLife == X
-- minBlockLife == O
-- blockLife == T
--   %%%%%%%%
-- % XXXOOXXX
-- % XXXOOXXX
-- %
-- %  T T T T
-- % T T T T
-- %
-- % XXXOOXXX
-- % XXXOOXXX

blockDescS 16 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife - 1)
                | x <- [0..blockColumns - 1]
                , y <- [midColumn - 1, midColumn]
                , (even x && y == midColumn) || (odd x && y == midColumn -1)
                ]

             ++ [ ((x, y), minBlockLife)
                | x <- [midColumn - 1, midColumn]
                , y <- [0, 1, blockRows - 2, blockRows - 1]
                ]

             ++ [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0, 1, blockRows - 2, blockRows - 1]
                , x /= midColumn - 1
                , x /= midColumn
                ]

    blockRows :: Num a => a
    blockRows = 8

    midRow :: Integral a => a
    midRow = blockRows `div` 2

    midColumn :: Integral a => a
    midColumn = blockColumns `div` 2

-- Level 17
-- maxBlockLife == X
-- minBlockLife == O
-- blockLife == T
--   %%%%%%%%
-- % XXXXXXXX
-- %  O O O O
-- %  O O O O
-- % T T T T
-- % T T T T
-- %  O O O O
-- %  O O O O
-- %
-- % XXX XXXX
blockDescS 17 = map (first adjustPos) allBlocks

  where

    allBlocks :: (Enum a, Num a, Eq a, Integral a) => [((a, a), Int)]
    allBlocks = [ ((x, y), maxBlockLife - 1)
                | x <- [0..blockColumns - 1]
                , y <- [3, 4]
                , odd x
                ]

             ++ [((x, y), minBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [1, 2, blockRows - 4, blockRows - 3]
                , even x
                ]

             ++ [ ((x, y), maxBlockLife)
                | x <- [0..blockColumns - 1]
                , y <- [0, blockRows - 1]
                , y == 0 || x /= midColumn
                ]

    blockRows :: Num a => a
    blockRows = 9

    midColumn :: Integral a => a
    midColumn = blockColumns `div` 2

blockDescS _ = error "No more levels"

-- Dynamic positioning/level size

adjustPos :: Integral a => (a, a) -> (Double, Double)
adjustPos = ((adjustHPos *** adjustVPos) . fI2)

adjustVPos :: Double -> Double
adjustVPos = (topMargin +) . ((blockHeight + blockSeparation) *)

adjustHPos :: Double -> Double
adjustHPos = (leftMargin +) . ((blockWidth + blockSeparation) *)

-- Fit as many as possible
blockColumns :: Num a => a
blockColumns =
    1 + round' ( (gameWidth - blockWidth - 2 * leftMargin)
               / (blockWidth + blockSeparation)
               )
  where
    round' = fromIntegral . floor

-- * Constants

topMargin :: Num a => a
topMargin = 10

leftMargin :: Num a => a
leftMargin = 25

-- * Auxiliary functions

fI2 :: Integral a => (a, a) -> (Double, Double)
fI2 (x, y) = (fromIntegral x, fromIntegral y)
