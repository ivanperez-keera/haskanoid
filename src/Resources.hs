{-# LANGUAGE CPP #-}
module Resources where

#ifdef sdl
import Game.AssetManager.SDL1
import Game.Audio.SDL
#endif
#ifdef sdl2
import Game.AssetManager.SDL2
import Game.Audio.SDL2
#endif

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
  , pointsUpImg :: Image
  , livesUpImg  :: Image
  , bgMusic     :: Maybe Music
  }
