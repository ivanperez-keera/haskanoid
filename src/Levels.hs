-- | Level definition
--
-- This module includes the definition of the levels.
--
-- Each level has:
--
--   - Logical properties (defined in 'blockPosS').
--
--   - Multimedia properties (a background and audio file).
--
-- Together they form 'levels'.
module Levels where

import Control.Arrow((***))
import Data.List (nub)
import Physics.TwoDimensions.Dimensions

import Constants
import Resources

-- * Levels
-- ** Level specification
data LevelSpec = LevelSpec
 { blockPoss  :: [Pos2D]       -- ^ Block positions
 , levelBg    :: ImageResource -- ^ Background image
 , levelMusic :: MusicResource -- ^ Background music
 }

-- | Number of levels. Change this in the code to finish
-- in a different level.
numLevels :: Int
numLevels = length levels

-- * Concrete levels
levels :: [LevelSpec]
levels = [ -- Level 0
           LevelSpec { blockPoss  = blockPosS 0
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level0.mp3"
                     }
         ,
           -- Level 1
           LevelSpec { blockPoss  = blockPosS 1
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 2
           LevelSpec { blockPoss  = blockPosS 2
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 3
           LevelSpec { blockPoss  = blockPosS 3
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level0.mp3"
                     }
         ,
           -- Level 4
           LevelSpec { blockPoss  = blockPosS 4
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 5
           LevelSpec { blockPoss  = blockPosS 5
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 6
           LevelSpec { blockPoss  = blockPosS 6
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level0.mp3"
                     }
         ,
           -- Level 7
           LevelSpec { blockPoss  = blockPosS 7
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 8
           LevelSpec { blockPoss  = blockPosS 8
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 9
           LevelSpec { blockPoss  = blockPosS 9
                     , levelBg    = Resource "data/level0.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 10
           LevelSpec { blockPoss  = blockPosS 10
                     , levelBg    = Resource "data/level1.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }
         ,
           -- Level 11
           LevelSpec { blockPoss  = blockPosS 11
                     , levelBg    = Resource "data/level2.png"
                     , levelMusic = Resource "data/level2.mp3"
                     }
         ,
           -- Level 12
           LevelSpec { blockPoss  = blockPosS 12
                     , levelBg    = Resource "data/level0.png"
                     , levelMusic = Resource "data/level1.mp3"
                     }         ]

-- | Level block specification (positions)

-- Level 0
--   %%%%%%%%
-- % XXXXXXXX
-- % XXXXXXXX
-- % XXXXXXXX
-- % XXXXXXXX
blockPosS :: Int -> [Pos2D]
blockPosS 0 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a) => [(a,a)]
       allBlocks = [(x,y) | x <- [0..blockColumns-1], y <- [0..blockRows-1]]

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
blockPosS 1 = map adjustPos allBlocks
 where
   allBlocks :: (Enum a, Num a, Eq a, Ord a) => [(a,a)]
   allBlocks = [ (x,y) | x <- [0..blockColumns-1], y <- [0..blockRows-1]
                       , (x + y > 2) && (x + y < 10)
               ]
               
   blockRows :: Num a => a
   blockRows = 6

-- Level 2
blockPosS 2  = map adjustPos allBlocks
 where
   allBlocks :: (Enum a, Num a, Eq a) => [(a,a)]
   allBlocks = [(x,y) | x <- [0..blockColumns - 1], y <- [0..blockRows-1]
                      , x /= y && (blockColumns - 1 - x) /= y]

   blockRows :: Num a => a
   blockRows = 4
               

-- Level 3
--   %%%%%%%%
-- %  X X X X
-- % X X X X
-- %  X X X X
-- % X X X X
blockPosS 3 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | x <- [0..blockColumns-1], y <- [0..blockRows-1],
                    ((even x) && (odd y) || (odd x) && (even y))]

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
blockPosS 4 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | x <- [0..blockColumns-1], y <- [0,blockRows-1]] ++
                   [(x,y) | x <- [0..blockColumns-1], y <- [2], odd x] ++
                   [(x,y) | x <- [0..blockColumns-1], y <- [4], even x]

       blockRows :: Num a => a
       blockRows = 7

-- Level 5
--   %%%%%%%%
-- %    X
-- %   X X
-- %  X   X
-- % XXXXXXX
blockPosS 5 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = nub $
         [(3,0),(blockColumns-4,1)] ++
         [(2,1),(blockColumns-3,1)] ++
         [(1,2),(blockColumns-2,2)] ++
         [(x,y) | x <- [0..blockColumns-1], y <- [3]]

-- Level 6
--   %%%%%%%%
-- %  XXXXXX
-- % X      X
-- % X      X
-- % X      X
-- %  XXXXXX
blockPosS 6 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | x <- [1..blockColumns-2], y <- [0,blockRows-1]] ++
                   [(x,y) | x <- [0,blockColumns-1],  y <- [1..blockRows-2]]

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
blockPosS 7 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | x <- [1..blockColumns-2], y <- [0,blockRows-1]] ++
                   [(x,y) | x <- [0,blockColumns-1],  y <- [1,blockRows-2]] ++
                   [(x,y) | x <- [3,4], y <- [2..4]]

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
blockPosS 8 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | x <- [0..blockColumns-1]
                          , y <- [0..blockRows-1]
                          , x /= 2, y /= 2 ]

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
blockPosS 9 = map ((adjustHPos *** adjustVPos) . fI2) allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | x <- [3], y <- [0..6]] ++
                   [(x,y) | x <- [0..6], y <- [3]] ++
                   [(x,y) | x <- [2,4], y <- [1,5]] ++
                   [(x,y) | x <- [1,5], y <- [2,4]]
                   
       adjustHPos :: Double -> Double
       adjustHPos = (leftMargin +) . ((blockWidth + blockSeparation) *)

       leftMargin :: Num a => a
       leftMargin = round' ((gameWidth - (blockWidth + blockSeparation) * 7)/2)
         where round' = fromIntegral . floor


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
blockPosS 10 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | x <- [0..blockColumns-1]
                          , y <- [0..blockRows-1], odd x] ++
                   [(x,y) | x <- [0..blockColumns-1], y <- [midRow], even x]

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
blockPosS 11 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | y <- [0..blockRows-1]
                          , x <- [0..(blockColumns-1) - 2 * abs (y - midRow)]]

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

blockPosS 12 = map adjustPos allBlocks

 where allBlocks :: (Enum a, Num a, Eq a, Integral a) => [(a,a)]
       allBlocks = [(x,y) | x <- [0..blockColumns-1], y <- [0]] ++
                   [(x,y) | x <- [2, blockColumns-3], y <- [1,2]] ++
                   [(1,4), (blockColumns-2,4), (0,5), (blockColumns-1,5)]

       blockRows :: Num a => a
       blockRows = 9

blockPosS _ = error "No more levels"

-- Dynamic positioning/level size

adjustPos :: Integral a => (a,a) -> (Double, Double)
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
  where round' = fromIntegral . floor

-- * Testing constants
--
-- These constans are used by the game, and can be mofidied on order to test
-- different levels.
--
-- TODO: should this be moved to the module Constants?

-- | Initial level. Change this in the code to start
-- from a different level.
initialLevel :: Int
initialLevel = 0

-- * Constants

topMargin :: Num a => a
topMargin = 10

leftMargin :: Num a => a
leftMargin = 25

-- * Auxiliary functions

fI2 :: Integral a => (a,a) -> (Double, Double)
fI2 (x,y) = (fromIntegral x, fromIntegral y)       
