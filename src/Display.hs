{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP                         #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances        #-}
module Display
  ( module Display
  , module ResourceManager
  )
  where

import Control.Arrow            ((***))
import Control.Monad
import Control.Monad.IfElse     (awhen)
import Data.Word
import FRP.Yampa.VectorSpace
import Game.Render.Renderer     as Render
import Game.Resource.Manager.Ref (getResourceFont, getResourceImage,
                                  prepareAllResources, tryGetResourceAudio)
import Game.VisualElem
import Graphics.UI.SDL          as SDL


import Constants
import GameState
import Objects
import ResourceManager as ResourceManager

#ifdef sdl

import Game.AssetManager.SDL1
import Game.Audio.SDL
import Graphics.UI.SDL.TTF.Types as TTF

import RenderSDL1

#elif sdl2
import Game.AssetManager.SDL2
import Game.Audio.SDL2
import Graphics.UI.SDL.TTF.Types as TTF

import RenderSDL2
#endif

-- * Initialization

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  initAudio

initGraphs :: ResourceManager.ResourceMgr -> IO RenderingCtx
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

  prepareAllResources mgr renderer

  return (renderer, window)
#endif

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music.
render :: ResourceMgr -> GameState -> RenderingCtx -> IO ()
render resourceManager shownState ctx = do
  audio   resourceManager shownState
  display (resourceManager, shownState) ctx

-- ** Audio

audio :: ResourceMgr -> GameState -> IO ()
audio resourceManager shownState = do
  -- Start bg music if necessary
  -- playing <- musicPlaying
  -- unless playing $ awhen (bgMusic resources) playMusic

  -- Play object hits
  mapM_ (audioObject resourceManager) $ gameObjects shownState

audioObject :: ResourceMgr -> Object -> IO ()
audioObject resourceManager object = when (objectHit object) $
  case objectKind object of
    Block -> do bhit <- tryGetResourceAudio resourceManager IdBlockHitFX undefined
                awhen bhit $ playFile
    _     -> return ()

-- ** Visual rendering
display :: Renderizable a RealRenderingCtx => a -> RenderingCtx -> IO ()
display a = onRenderingCtx $ \ctx -> 
  Render.render ctx a (0, 0)

instance Renderizable (ResourceMgr, GameState) RealRenderingCtx where
  render screen (resources, shownState) (baseX, baseY) = do

    -- Render background
    img <- getResourceImage resources IdBgImg undefined
    Render.render screen (resources, img) (0, 0)

    Render.render screen (resources, gameInfo shownState) (0, 0)
    Render.render screen (resources, gameStatus (gameInfo shownState)) (gameLeft, gameTop)
    mapM_ (\obj -> Render.render screen (resources, obj) (gameLeft, gameTop)) $ gameObjects shownState

instance Renderizable (ResourceMgr, GameInfo) RealRenderingCtx where

 -- Paint HUD
 render ctx (resources, over) (baseX, baseY) = do
   let message1 = (resources, "Level: " ++ show (gameLevel over))
   h1 <- renderHeight message1

   renderAlignLeft  ctx message1                                          (10, 10)
   renderAlignLeft  ctx (resources, "Points: " ++ show (gamePoints over)) (10, 10 + h1 + 5)
   renderAlignRight ctx (resources, "Lives: "  ++ show (gameLives over))  (10, 10)

instance Renderizable (ResourceMgr, GameStatus) RealRenderingCtx where
  renderTexture screen (resources, status) =
    renderTexture screen (resources, statusMsg status)

  renderSize (resources, status) =
    renderSize (resources, statusMsg status)

  render screen txt (x, y) = do
    txt <- renderTexture screen txt
    renderAlignCenter screen txt (x, y)

statusMsg :: GameStatus -> Maybe String
statusMsg GamePlaying          = Nothing
statusMsg GamePaused           = Just "Paused"
statusMsg (GameLoading n name) = Just ("Level " ++ name)
statusMsg GameOver             = Just "GAME OVER!!!"
statusMsg GameFinished         = Just "You won!!! Well done :)"
statusMsg GameStarted          = Nothing

instance Renderizable (ResourceMgr, Object) RealRenderingCtx where

  renderTexture r (res, obj) = do img <- getResourceImage res (objectImage obj) undefined
                                  renderTexture r img

  renderSize = return . (round *** round) . objectSize . snd

  render screen (resources, object) base =
    case objectKind object of
      (Side {}) -> return ()
      other     -> do tex <- renderTexture screen (resources, object)
                      Render.render screen tex (x,y)

    where (x,y) = (round *** round) (objectTopLevelCorner object ^+^ base')
          base' = (fromIntegral *** fromIntegral) base

-- Partial function. Object has image.
objectImage :: Object -> ResourceId
objectImage object = case objectKind object of
  Paddle           -> IdPaddleImg
  Block            -> let (BlockProps e _) = objectProperties object 
                      in case e of 
                           3 -> IdBlock1Img
                           2 -> IdBlock2Img
                           n -> IdBlock3Img
  Ball             -> IdBallImg
  PowerUp PointsUp -> IdPointsUpImg
  PowerUp LivesUp  -> IdLivesUpImg
  PowerUp NothingUp -> IdNothingUpImg
  PowerUp DestroyUp -> IdDestroyUpImg

-- TODO: Change this ugly constraint.
instance 
#ifdef sdl
  Renderizable (TTF.Font, String, (Word8, Word8, Word8)) ctx
#elif sdl2
  Renderizable (TTF.TTFFont, String, (Word8, Word8, Word8)) ctx
#endif
  => Renderizable (ResourceMgr, String) ctx where

  renderTexture surface (resources, msg) = do
    font <- unFont <$> getResourceFont resources IdGameFont undefined
    renderTexture surface (font, msg, (128 :: Word8, 128 :: Word8, 128 :: Word8))

  renderSize (resources, msg) = do
    font <- unFont <$> getResourceFont resources IdGameFont undefined
    renderSize (font, msg, (128 :: Word8, 128 :: Word8, 128 :: Word8))
