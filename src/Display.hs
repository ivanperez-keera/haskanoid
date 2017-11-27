{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP                         #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeSynonymInstances        #-}
module Display
  ( module Display
  , module ResourceManager
  )
  where

import Control.Arrow         ((***))
import Control.Monad
import Control.Monad.IfElse  (awhen)
import FRP.Yampa.VectorSpace
import Game.Render.Renderer  as Render
import Graphics.UI.SDL       as SDL

import Constants
import GameState
import Objects
import Resources
import ResourceManager as ResourceManager

#ifdef sdl
import Game.AssetManager.SDL1
import Game.Audio.SDL

import RenderSDL1
#elif sdl2
import Data.IORef
import Game.AssetManager.SDL2
import Game.Audio.SDL2

import RenderSDL2
#endif

-- * Initialization

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  initAudio

initGraphs :: ResourceMgr -> IO RenderingCtx
#ifdef sdl
initGraphs _mgr = do
  -- Create window
  screen <- SDL.setVideoMode width height 32 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor False

#elif sdl2

initGraphs mgr = do
  -- Create window
  (window,renderer) <- SDL.createWindowAndRenderer (Size width height) [WindowShown, WindowOpengl]
  renderSetLogicalSize renderer width height

  preloadResources mgr renderer

  return (renderer, window)
#endif

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music.
render :: ResourceMgr -> GameState -> RenderingCtx -> IO ()
render resourceManager shownState ctx = do
  res <- loadNewResources resourceManager shownState
  audio   res shownState
  display (res, shownState) ctx

-- ** Audio

audio :: Resources -> GameState -> IO ()
audio resources shownState = do
  -- Start bg music if necessary
  playing <- musicPlaying
  unless playing $ awhen (bgMusic resources) playMusic

  -- Play object hits
  mapM_ (audioObject resources) $ gameObjects shownState

audioObject :: Resources -> Object -> IO ()
audioObject resources object = when (objectHit object) $
  case objectKind object of
    Block -> playFile (blockHitSnd resources)
    _     -> return ()

-- ** Visual rendering
display :: Renderizable a RealRenderingCtx => a -> RenderingCtx -> IO ()
display a = onRenderingCtx $ \ctx -> 
  Render.render ctx a (0, 0)

instance Renderizable (Resources, GameState) RealRenderingCtx where
  render screen (resources, shownState) (baseX, baseY) = do

    -- Render background
    Render.render screen (resources, bgImage resources) (0, 0)

    Render.render screen (resources, gameInfo shownState) (0, 0)
    Render.render screen (resources, gameStatus (gameInfo shownState)) (gameLeft, gameTop)
    mapM_ (\obj -> Render.render screen (resources, obj) (gameLeft, gameTop)) $ gameObjects shownState

instance Renderizable (Resources, GameInfo) RealRenderingCtx where

 -- Paint HUD
 render ctx (resources, over) (baseX, baseY) = do
   let message1 = (resources, "Level: " ++ show (gameLevel over))
   h1 <- renderHeight message1

   renderAlignLeft  ctx message1                                          (10, 10)
   renderAlignLeft  ctx (resources, "Points: " ++ show (gamePoints over)) (10, 10 + h1 + 5)
   renderAlignRight ctx (resources, "Lives: "  ++ show (gameLives over))  (10, 10)

instance Renderizable (Resources, GameStatus) RealRenderingCtx where
  renderTexture screen (resources, status) =
    renderTexture screen (resources, statusMsg status)

  renderSize (resources, status) =
    renderSize (resources, statusMsg status)

  render screen txt (x, y) = do
    txt <- renderTexture screen txt
    renderAlignCenter screen txt (x, y)

statusMsg GamePlaying     = Nothing
statusMsg GamePaused      = Just "Paused"
statusMsg (GameLoading n) = Just ("Level " ++ show n)
statusMsg GameOver        = Just "GAME OVER!!!"
statusMsg GameFinished    = Just "You won!!! Well done :)"

instance Renderizable (Resources, Object) RealRenderingCtx where
  renderTexture r  = renderTexture r . objectImage
  renderSize       = return . (round *** round) . objectSize . snd
  render screen (resources, object) base =
    case objectKind object of
      (Side {}) -> return ()
      other     -> do tex <- renderTexture screen (resources, object)
                      Render.render screen tex (x,y)

    where (x,y) = (round *** round) (objectTopLevelCorner object ^+^ base')
          base' = (fromIntegral *** fromIntegral) base

-- Partial function. Object has image.
objectImage :: (Resources, Object) -> Image
objectImage (resources, object) = case objectKind object of
  Paddle           -> paddleImg resources
  Block            -> let (BlockProps e _) = objectProperties object 
                      in case e of 
                           3 -> block1Img resources
                           2 -> block2Img resources
                           n -> block3Img resources
  Ball             -> ballImg resources
  PowerUp PointsUp -> pointsUpImg resources
  PowerUp LivesUp  -> livesUpImg  resources
