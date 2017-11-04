module Resources where

import Game.AssetManager.SDL1
import Game.Audio.SDL

-- | Includes all the assets needed at the current time in the game.
data Resources = Resources
  { resFont     :: Font
  , blockHitSnd :: Audio
  , bgImage     :: Maybe Image
  , ballImg     :: Image
  , block1Img   :: Image
  , block2Img   :: Image
  , block3Img   :: Image
  , paddleImg   :: Image
  , bgMusic     :: Maybe Music
  }
