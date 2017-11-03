-- | Level definition
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

import Control.Arrow                    ((***), first)
import Data.List                        (nub)
import Game.AssetManager
import Physics.TwoDimensions.Dimensions

import Constants

-- * Levels
-- ** Level specification
data LevelSpec = LevelSpec
 { blockCfgs  :: [(Pos2D, Int)] -- ^ Block positions and block lives
 , levelBg    :: ImageResource  -- ^ Background image
 , levelMusic :: MusicResource  -- ^ Background music
 }

-- | Number of levels. Change this in the code to finish
-- in a different level.
numLevels :: Int
numLevels = length levels

-- * Concrete levels
levels :: [LevelSpec]
levels = map (\(d,b,m) -> LevelSpec d (Resource b) (Resource m))
  [ (blockDescS 0,  "data/level1.png", "data/level0.mp3")
  , (blockDescS 1,  "data/level1.png", "data/level1.mp3")
  , (blockDescS 2,  "data/level2.png", "data/level2.mp3")
  , (blockDescS 3,  "data/level1.png", "data/level0.mp3")
  , (blockDescS 4,  "data/level1.png", "data/level1.mp3")
  , (blockDescS 5,  "data/level2.png", "data/level2.mp3")
  , (blockDescS 6,  "data/level1.png", "data/level0.mp3")
  , (blockDescS 7,  "data/level1.png", "data/level1.mp3")
  , (blockDescS 8,  "data/level2.png", "data/level2.mp3")
  , (blockDescS 9,  "data/level0.png", "data/level1.mp3")
  , (blockDescS 10, "data/level1.png", "data/level1.mp3")
  , (blockDescS 11, "data/level2.png", "data/level2.mp3")
  , (blockDescS 12, "data/level0.png", "data/level1.mp3")
  , (blockDescS 13, "data/level1.png", "data/level1.mp3")
  , (blockDescS 14, "data/level2.png", "data/level2.mp3")
  , (blockDescS 15, "data/level1.png", "data/level0.mp3")
  , (blockDescS 16, "data/level1.png", "data/level1.mp3")
  , (blockDescS 17, "data/level2.png", "data/level2.mp3")
  ]

-- | Level block specification (positions,lives of block)

-- Level 0
--   %%%%%%%%
-- % XXXXXXXX
-- % XXXXXXXX
-- % XXXXXXXX
-- % XXXXXXXX
blockDescS :: Int -> [(Pos2D, Int)]
blockDescS 0 = map (first adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                          , y <- [0..blockRows - 1]
                   ]

       blockRows :: Int
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
   allBlocks :: [((Int, Int), Int)]
   allBlocks = [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                      , y <- [0..blockRows - 1]
                                      , (x + y > 2) && (x + y < 10)
               ]
               
   blockRows :: Int
   blockRows = 6

-- Level 2
blockDescS 2  = map (first adjustPos) allBlocks
 where
   allBlocks :: [((Int, Int), Int)]
   allBlocks = [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                      , y <- [0..blockRows - 1]
                                      , x /= y && (blockColumns - 1 - x) /= y
               ]

   blockRows :: Int
   blockRows = 4
               

-- Level 3
--   %%%%%%%%
-- %  X X X X
-- % X X X X
-- %  X X X X
-- % X X X X
blockDescS 3 = map (first adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                          , y <- [0..blockRows - 1]
                                          , (even x && odd y)  ||
                                            (odd x  && even y)
                   ]

       blockRows :: Int
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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                          , y <- [0,blockRows - 1]]
                   ++ [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                             , y <- [2], odd x]
                   ++ [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                             , y <- [4], even x]

       blockRows :: Int
       blockRows = 7

-- Level 5
--   %%%%%%%%
-- %    X
-- %   X X
-- %  X   X
-- % XXXXXXX
blockDescS 5 = map (first adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = nub $
         [((3,0), maxBlockLife),((blockColumns - 4,1), maxBlockLife)]
         ++ [((2,1), maxBlockLife),((blockColumns - 3,1), maxBlockLife)]
         ++ [((1,2), maxBlockLife),((blockColumns - 2,2), maxBlockLife)]
         ++ [((x,y), maxBlockLife) | x <- [0..blockColumns - 1], y <- [3]]

-- Level 6
--   %%%%%%%%
-- %  XXXXXX
-- % X      X
-- % X      X
-- % X      X
-- %  XXXXXX
blockDescS 6 = map (first adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- [1..blockColumns - 2]
                                          , y <- [0, blockRows - 1]]
                   ++ [((x,y), maxBlockLife) | x <- [0, blockColumns - 1]
                                             , y <- [1..blockRows - 2]]

       blockRows :: Int
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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- [1..blockColumns - 2]
                                          , y <- [0, blockRows - 1]]
                   ++ [((x,y), maxBlockLife) | x <- [0, blockColumns - 1]
                                             ,  y <- [1, blockRows - 2]]
                   ++ [((x,y), maxBlockLife) | x <- [3,4]
                                             , y <- [2..4]]

       blockRows :: Int
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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                          , y <- [0..blockRows - 1]
                                          , x /= 2, y /= 2
                   ]

       blockRows :: Int
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
blockDescS 9 = map (first adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- midColumns, y <- [0..6]]
                   ++ [((x,y), maxBlockLife) | x <- [0..blockColumns-1], y <- [3]]
                   ++ [((x,y), maxBlockLife) | x <- [2,blockColumns-3], y <- [1,5]]
                   ++ [((x,y), maxBlockLife) | x <- [1,blockColumns-2], y <- [2,4]]

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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks =  [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                           , y <- [0..blockRows - 1]
                                           , odd x || (y == midRow && even x)]
  
       blockRows :: Int
       blockRows = 9

       midRow :: Int
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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | y <- [0..blockRows-1]
                                          , x <- [0..(blockColumns-1)
                                                 - (2 * abs (y - midRow))]
                   ]

       blockRows :: Int
       blockRows = 7

       midRow :: Int
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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                          , y <- [0]]
                   ++ [((x,y), maxBlockLife) | x <- [2, blockColumns - 3]
                                             , y <- [1, 2]]
                   ++ [ ((1,4), maxBlockLife)
                      , ((blockColumns - 2,4), maxBlockLife)
                      , ((0,5), maxBlockLife)
                      , ((blockColumns - 1,5), maxBlockLife)
                      ]

-- Level 13
-- X == maxBlockLife
-- O == maxBlockLife - 1
--   %%%%%%%%
-- % XOXOXOXO
-- % OXOXOXOX
-- % XOXOXOXO
-- % OXOXOXOX
blockDescS 13 = map (first adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), blockLife) | x <- [0..blockColumns - 1]
                                       , y <- [0..blockRows - 1]
                                       , let blockLife = if even (x + y)
                                                           then maxBlockLife
                                                           else maxBlockLife - 1
                                       ]
                   
       blockRows :: Int
       blockRows = 4

-- Level 14
-- X == maxBlockLife
-- O == maxBlockLife - 1
-- Y == minBlockLife
--   %%%%%%%%
-- % YYYYYYYY
-- % YXXXXXXY
-- % YXOOOOXY
-- % YXXXXXXY
-- % YYYYYYYY
blockDescS 14 = map (first adjustPos) allBlocks

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), minBlockLife) | x <- [0..blockColumns - 1]
                                          , y <- [0..blockRows - 1]
                                          , (x == 0)
                                            || (y == 0)
                                            || (x == blockColumns - 1)
                                            || (y == blockRows - 1)]
                   ++ [((x,y), maxBlockLife - 1) | x <- [1..blockColumns - 2]
                                                 , y <- [1..blockRows - 2]
                                                 , (x == 1)
                                                   || (y == 1)
                                                   || (x == blockColumns - 2)
                                                   || (y == blockRows - 2)]
                   ++ [((x,y), maxBlockLife) | x <- [2..blockColumns - 3]
                                          , y <- [2..blockRows - 3]
                                          , (x == 2)
                                            || (y == 2)
                                            || (x == blockColumns - 3)
                                            || (y == blockRows - 3)]
                   
       blockRows :: Int
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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                          , y <- [0..midRow], odd x]
                   ++ [((x,y), minBlockLife) | x <- [0..blockColumns - 1]
                                             , y <- [midRow], even x]
                   ++ [((x,y), maxBlockLife - 1) | x <- [0..blockColumns - 1]
                                                 , y <- [midRow
                                                         + 1..blockColumns - 1]
                                                 , even x]
  
       blockRows :: Int
       blockRows = 9

       midRow :: Int
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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife - 1) | x <- [0..blockColumns - 1]
                                              , y <- midColumns
                                              , (even x && y == midColumn)
                                                || (odd x && y == midColumn -1)]
                   ++ [((x,y), minBlockLife) | x <- [midColumn - 1, midColumn]
                                             , y <- [0, 1,
                                                     blockRows - 2, blockRows - 1]]
                   ++ [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                             , y <- [0, 1,
                                                     blockRows - 2, blockRows - 1]
                                             , x /= midColumn - 1, x /= midColumn]


       blockRows :: Int
       blockRows = 8

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

 where allBlocks :: [((Int, Int), Int)]
       allBlocks = [((x,y), maxBlockLife - 1) | x <- [0..blockColumns - 1]
                                              , y <- [3, 4]
                                              , odd x]
                   ++ [((x,y), minBlockLife) | x <- [0..blockColumns - 1]
                                             , y <- [1, 2, 
                                                     blockRows - 4, blockRows - 3]
                                             , even x ]
                   ++ [((x,y), maxBlockLife) | x <- [0..blockColumns - 1]
                                             , y <- [0, blockRows - 1]
                                             , y == 0 || x /= midColumn]
       blockRows :: Int
       blockRows = 9

blockDescS _ = error "No more levels"

-- Dynamic positioning/level size

adjustPos :: (Int, Int) -> (Double, Double)
adjustPos = (adjustHPos . fI) *** (adjustVPos . fI)
  where
    adjustVPos = (topMargin +) . ((blockHeight + blockSeparation) *)
    adjustHPos = (leftMargin +) . ((blockWidth + blockSeparation) *)
    fI         = fromIntegral

-- Fit as many as possible
blockColumns :: Int
blockColumns =
    1 + floor ( (gameWidth - blockWidth - 2 * minLeftMargin)
              / (blockWidth + blockSeparation)
              )

midColumn :: Int
midColumn = blockColumns `div` 2

midColumns :: [Int]
midColumns | odd blockColumns = [midColumn]
           | otherwise        = [midColumn - 1, midColumn]

-- * Constants

topMargin :: Num a => a
topMargin = 10

minLeftMargin :: Num a => a
minLeftMargin = 25

leftMargin :: Double
leftMargin = round' $
  (gameWidth - blockWidth - (blockWidth + blockSeparation) * (fromIntegral blockColumns - 1)) / 2
  where
    round' :: Double -> Double
    round' x = fromIntegral (floor x :: Int)
