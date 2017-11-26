{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances        #-}
module Display
  ( module Display
  , module ResourceManager
  )
  where

import Control.Monad
import Control.Monad.IfElse
import Data.Tuple.Extra
import Data.Word
import Game.AssetManager.SDL1
import Game.Audio.SDL
import Game.Render.Renderer   as Renderer
import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.TTF    as TTF

import Constants
import GameState
import Objects
import Resources
import ResourceManager

type RenderingCtx = ()
type RealRenderingCtx = Surface

-- * Initialization

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  initAudio

initGraphs :: ResourceMgr -> IO RenderingCtx
initGraphs _mgr = do
  -- Create window
  screen <- SDL.setVideoMode width height 32 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor False

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music.
render :: ResourceMgr -> GameState -> RenderingCtx -> IO ()
render resourceManager shownState ctx = do
  res <- loadNewResources resourceManager shownState
  audio   res shownState
  display res shownState ctx

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
    Block -> playFile (blockHitSnd resources) 3000
    _     -> return ()

-- ** Visual rendering
display :: Resources -> GameState -> RenderingCtx -> IO ()
display resources shownState () = do
  -- Obtain surface
  screen <- getVideoSurface
  Renderer.render screen (resources, shownState) (0, 0)

instance Renderizable (Resources, GameInfo) RealRenderingCtx where

 render ctx (resources, over) (baseX, baseY) = do
   -- paintInfo :: Surface -> Resources -> GameInfo -> IO ()
   -- paintInfo ctx resources over = void $ do
   -- clearScreen ctx (0x11, 0x22, 0x33)

   -- Paint HUD
   let message1 = (resources, "Level: " ++ show (gameLevel over))
   h1 <- renderHeight message1

   renderAlignLeft  ctx message1                                          (10, 10)
   renderAlignLeft  ctx (resources, "Points: " ++ show (gamePoints over)) (10, 10 + h1 + 5)
   renderAlignRight ctx (resources, "Lives: "  ++ show (gameLives over))  (10, 10)

instance Renderizable (Resources, GameStatus) RealRenderingCtx where
  renderTexture screen (resources, status) =
    renderTexture screen (resources, msg status)
    where 
      msg GamePlaying     = Nothing
      msg GamePaused      = Just "Paused"
      msg (GameLoading n) = Just ("Level " ++ show n)
      msg GameOver        = Just "GAME OVER!!!"
      msg GameFinished    = Just "You won!!! Well done :)"

  renderSize (resources, status) = do
    screen <- getVideoSurface
    msg    <- renderTexture screen (resources, status)
    renderSize msg

  render screen txt (x, y) = do
    txt <- renderTexture screen txt
    renderAlignCenter screen txt (0, 0)

instance Renderizable (Resources, Object) RealRenderingCtx where
  render screen (resources, object) (baseX, baseY) =
      renderTexture screen (resources, object) >>= \x -> Renderer.render screen x p'
    where
      p'     = (baseX + x, baseY + y)
      (x, y) = both round $ objectTopLevelCorner object

  renderTexture screen (resources, object) = 
    renderTexture screen (resources, objectImg resources)

    where

      objectImg = case objectKind object of
        Paddle           -> Just . paddleImg
        Block            -> let (BlockProps e _) = objectProperties object
                            in Just . blockImgF e
        Ball             -> Just . ballImg
        PowerUp PointsUp -> Just . pointsUpImg
        PowerUp LivesUp  -> Just . livesUpImg
        Side             -> \_ -> Nothing 

      blockImgF 3 = block1Img
      blockImgF 2 = block2Img
      blockImgF n = block3Img

  renderSize (resources, object) = do
    screen <- getVideoSurface
    msg    <- renderTexture screen (resources, object)
    renderSize msg

-- * Auxiliary drawing functions

instance Renderizable (Resources, GameState) RealRenderingCtx where
  render screen (resources, shownState) (baseX, baseY) = do

    -- Render background
    -- Renderer.render screen (imgSurface <$> bgImage resources) (0, 0)
    Renderer.render screen (resources, bgImage resources) (0, 0)
    Renderer.render screen (resources, gameInfo shownState) (0, 0)
    Renderer.render screen (resources, gameStatus (gameInfo shownState)) (gameLeft, gameTop)
    mapM_ (\obj -> Renderer.render screen (resources, obj) (gameLeft, gameTop)) $ gameObjects shownState

    -- Double buffering
    SDL.flip screen

-- * SDL Specific instances

instance Renderizable x Surface => Renderizable (Maybe x) Surface where
  renderTexture surface Nothing  = return Nothing
  renderTexture surface (Just x) = renderTexture surface x
  renderSize Nothing  = return (0, 0)
  renderSize (Just x) = renderSize x

instance Renderizable (res, a) Surface => Renderizable (res, Maybe a) Surface where

  renderTexture _surface (res, Nothing) = return Nothing
  renderTexture surface  (res, Just x)  = renderTexture surface (res, x)
  renderSize (resources, Nothing) = return (0, 0)
  renderSize (resources, Just x)  = renderSize (resources, x)

instance Renderizable (Resources, Image) Surface where
  renderTexture ctx (resources, img) = renderTexture ctx (imgSurface img)
  renderSize (resources, img) = renderSize (imgSurface img)

instance Renderizable (TTF.Font, String, (Word8, Word8, Word8)) ctx
         => Renderizable (Resources, String) ctx where

  renderTexture surface (resources, msg) = do
    let font = unFont $ resFont resources
    renderTexture surface (font, msg, (128 :: Word8, 128 :: Word8, 128 :: Word8))

  renderSize (resources, msg) = do
    screen <- getVideoSurface
    msg    <- renderTexture screen (resources, msg)
    renderingSize screen msg

instance Renderizable (TTF.Font, String, (Word8, Word8, Word8)) Surface where

  renderTexture _surface (font, msg, (r,g,b)) = do
    message <- TTF.renderTextSolid font msg (SDL.Color r g b)
    return (Just message)

  renderSize (font, msg, color) = do
    screen <- getVideoSurface
    msg    <- renderTexture screen (font, msg, color)
    renderingSize screen msg

instance Renderizable Surface Surface where

  renderTexture _surface surface = return $ Just surface

  renderSize surface = do
    screen <- getVideoSurface
    renderingSize screen (Just surface)

instance RenderingContext Surface where
  type RenderingUnit Surface = Maybe Surface
  renderingWidth  screen screen' = return $ maybe 0 surfaceGetWidth screen'
  renderingHeight screen screen' = return $ maybe 0 surfaceGetHeight screen'
  renderingScreenSize screen = renderingSize screen (Just screen)
  renderUnit screen Nothing        (x,y) = return ()
  renderUnit screen (Just surface) (x,y) = void $
    SDL.blitSurface surface Nothing screen $ Just (SDL.Rect x y (-1) (-1))

clearScreen screen (r,g,b) = do
  -- Clear background
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format r g b
  fillRect screen Nothing bgColor
