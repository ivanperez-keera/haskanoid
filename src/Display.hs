{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE TypeFamilies                #-}
module Display
  ( module Display
  , module ResourceManager
  )
  where

import Control.Monad
import Control.Monad.IfElse
import Data.Tuple.Extra
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

instance Renderizable (Resources, GameInfo) Surface where

 render screen (resources, over) (baseX, baseY) = do
   -- paintInfo :: Surface -> Resources -> GameInfo -> IO ()
   -- paintInfo screen resources over = void $ do
   -- clearScreen screen (0x11, 0x22, 0x33)

   -- Paint HUD
   let message1 = (resources, "Level: " ++ show (gameLevel over))
   h1 <- renderHeight message1

   renderAlignLeft  screen message1                                          (10, 10)
   renderAlignLeft  screen (resources, "Points: " ++ show (gamePoints over)) (10, 10 + h1 + 5)
   renderAlignRight screen (resources, "Lives: "  ++ show (gameLives over))  (10, 10)

instance Renderizable (Resources, GameStatus) Surface where
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

instance Renderizable (Resources, Object) Surface where
  render screen (resources, object) (baseX, baseY) =
      renderTexture screen (resources, object) >>= \x -> renderUnit screen x p'
    where
      p'     = (baseX + x, baseY + y)
      (x, y) = both round $ objectTopLevelCorner object

  renderTexture screen (resources, object) = 
    case objectKind object of
      Side -> return Nothing
      _    -> return $ Just bI

    where

      bI = imgSurface $ objectImg resources

      objectImg = case objectKind object of
        Paddle           -> paddleImg
        Block            -> let (BlockProps e _) = objectProperties object
                            in blockImgF e
        Ball             -> ballImg
        PowerUp PointsUp -> pointsUpImg
        PowerUp LivesUp  -> livesUpImg

      blockImgF 3 = block1Img
      blockImgF 2 = block2Img
      blockImgF n = block3Img

  renderSize (resources, object) = do
    screen <- getVideoSurface
    msg    <- renderTexture screen (resources, object)
    renderSize msg

-- * Auxiliary drawing functions

instance Renderizable (Resources, GameState) Surface where
  render screen (resources, shownState) (baseX, baseY) = do

    -- Render background
    Renderer.render screen (imgSurface <$> bgImage resources) (0, 0)
    Renderer.render screen (resources, gameInfo shownState) (0, 0)
    Renderer.render screen (resources, gameStatus (gameInfo shownState)) (gameLeft, gameTop)
    mapM_ (\obj -> Renderer.render screen (resources, obj) (gameLeft, gameTop)) $ gameObjects shownState

    -- Double buffering
    SDL.flip screen

instance Renderizable x Surface => Renderizable (Maybe x) Surface where
  renderTexture surface Nothing  = return Nothing
  renderTexture surface (Just x) = renderTexture surface x
  renderSize Nothing  = return (0, 0)
  renderSize (Just x) = renderSize x

instance Renderizable Surface Surface where

  renderTexture _surface surface = return $ Just surface

  renderSize surface = do
    screen <- getVideoSurface
    renderingSize screen (Just surface)

instance Renderizable (Resources, String) Surface where

  renderTexture _surface (resources, msg) = do
    let font = unFont $ resFont resources
    message <- TTF.renderTextSolid font msg (SDL.Color 128 128 128)
    return (Just message)

  renderSize (resources, msg) = do
    screen <- getVideoSurface
    msg    <- renderTexture screen (resources, msg)
    renderingSize screen msg

instance Renderizable (res, a) Surface => Renderizable (res, Maybe a) Surface where

  renderTexture _surface (res, Nothing) = return Nothing
  renderTexture surface  (res, Just x)  = renderTexture surface (res, x)
  renderSize (resources, Nothing) = return (0, 0)
  renderSize (resources, Just x)  = renderSize (resources, x)

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
