{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP                         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
module Display
  ( module Display
  , module ResourceManager
  )
  where

import App.Context               (RuntimeContext)
import Control.DeepSeq           (NFData, rnf)
import Control.Monad
import Control.Monad.IfElse      (awhen)
import Game.Audio
import Game.Resource.Manager.Ref (tryGetResourceMusic, tryGetResourceSound)
import Game.VisualElem
import Graphics.UI.Align
import Graphics.UI.Collage
import Graphics.UI.SDL           as SDL
import Playground                (displayWithBGColorImage')
import Playground.SDL            (RenderingCtx, dAlignToAbsPos')

import Constants
import GameState
import Objects
import ResourceManager

#ifdef sdl2
import Game.Render.Monad.SDL ()
#endif

-- * Environment handling

-- | The rendering environment given by the resource manager,
-- runtime context, and rendering context.
--
-- This is a valid 'GRenderingEnv' which has functions like
-- renderingEnvResourceMgr, renderingEnvRuntimeCtx and
-- renderingEnvRuntimeCtx.
type RenderEnv = (ResourceMgr, RuntimeContext, RenderingCtx)

-- Orphan instaces

-- | NFData instance for Surface.
instance NFData Surface where
  rnf s = s `seq` ()

#ifdef sdl2
-- | NFData instances for Texture.
instance NFData Texture where
  rnf s = s `seq` ()
#endif

-- * Initialization

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  initAudio

adjustSDLsettings :: IO ()
#ifdef sdl
adjustSDLsettings = void $ do
  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor False
#elif sdl2
adjustSDLsettings = return ()
#endif

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music.
render :: GameState -> RenderEnv -> IO ()
render shownState env = do
  audio   shownState env
  display shownState env

-- ** Audio

audio :: GameState -> RenderEnv -> IO ()
audio shownState (resourceManager, _, _) = do
  -- Start bg music if necessary
  playing <- musicIsPlaying
  unless playing $ do
    m <- tryGetResourceMusic resourceManager IdBgMusic undefined
    awhen m playMusic

  -- Play object hits
  mapM_ (audioObject resourceManager) $ gameObjects shownState

audioObject :: ResourceMgr -> Object -> IO ()
audioObject resourceManager object = when (objectHit object) $
  case objectKind object of
    Block -> do bhit <- tryGetResourceSound resourceManager IdBlockHitFX undefined
                awhen bhit playSoundFX
    _     -> return ()

-- ** Visual rendering
display :: GameState -> RenderEnv -> IO ()
display shownState env = do
  let (cCol, mBgImg, clg) = game shownState
  clg' <- collageMapM (dAlignToAbsPos' env) clg
  displayWithBGColorImage' (cCol, mBgImg, clg') env

game :: GameState -> (ResourceId, Maybe ResourceId, Collage (VisualElem ResourceId) ((Int, Int), Align))
game shownState = (IdBlack, Just IdBgImg, mconcat [levelTxt, pointsTxt, livesTxt, mconcat mStatusTxt, mconcat objItems] )
  where
    -- HUD
    levelTxt   = CollageItem (VisualText IdGameFont IdGameFontColor ("Level: "  ++ show (gameLevel over)))
                             ((10, 10), Align HLeft  VTop)
    pointsTxt  = CollageItem (VisualText IdGameFont IdGameFontColor ("Points: " ++ show (gamePoints over)))
                             ((10, 40), Align HLeft  VTop)
    livesTxt   = CollageItem (VisualText IdGameFont IdGameFontColor ("Lives: "  ++ show (gameLives over)))
                             ((10, 40), Align HRight VTop)
    -- Game status
    mStatusTxt = [ CollageItem (VisualText IdGameFont IdGameFontColor msg)
                               ((0, 0), Align HCenter VCenter)
                 | Just msg <- [ statusMsg status ] ]

    -- Game objects
    objItems = map objItem $ filter (not . isSide) $ gameObjects shownState
    objItem object = CollageItem (VisualImage (objectImage object))
                                 (pos, Align HLeft VTop)
      where
        pos      = (round (ox + gameLeft), round (oy + gameTop))
        (ox, oy) = objectTopLevelCorner object

    over   = gameInfo shownState
    status = gameStatus over

statusMsg :: GameStatus -> Maybe String
statusMsg GamePlaying               = Nothing
statusMsg GamePaused                = Just "Paused"
statusMsg (GameLoading _ levelName) = Just ("Level " ++ levelName)
statusMsg GameOver                  = Just "GAME OVER!!!"
statusMsg GameFinished              = Just "You won!!! Well done :)"
statusMsg GameStarted               = Nothing

-- Partial function. Object has image.
objectImage :: Object -> ResourceId
objectImage object = case objectKind object of
  Paddle                -> IdPaddleImg
  Block                 -> let (BlockProps e pu _) = objectProperties object
                           in if pu
                                then IdBlockPuImg -- signals powerup
                                else case e of
                                       3 -> IdBlock1Img
                                       2 -> IdBlock2Img
                                       _ -> IdBlock3Img
  Ball                  -> IdBallImg
  PowerUp PointsUp      -> IdPointsUpImg
  PowerUp LivesUp       -> IdLivesUpImg
  PowerUp MockUp        -> IdMockUpImg
  PowerUp DestroyBallUp -> IdDestroyBallUpImg
