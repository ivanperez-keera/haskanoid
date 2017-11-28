{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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
import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Align  as SDL
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

initGraphs :: ResourceMgr -> IO ()
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
render :: ResourceMgr -> GameState -> RenderingCtx -> IO()
render resourceManager shownState () = do
  resources <- loadNewResources resourceManager shownState
  audio   resources shownState
  display resources shownState

-- ** Audio

audio :: Resources -> GameState -> IO()
audio resources shownState = do
  -- Start bg music if necessary
  playing <- musicPlaying
  unless playing $ awhen (bgMusic resources) playMusic

  -- Play object hits
  mapM_ (audioObject resources) $ gameObjects shownState

audioObject :: Resources -> Object -> IO ()
audioObject resources object = when (objectHit object) $
  case objectKind object of
    (Block) -> playFile (blockHitSnd resources) 3000
    _       -> return ()

-- ** Painting

display :: Resources -> GameState -> IO()
display resources shownState = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint background
  awhen (bgImage resources) $ \bg' -> void $ do
    let bg = imgSurface bg'
    SDL.blitSurface bg Nothing screen $ Just (SDL.Rect 0 0 (-1) (-1))

  hud <- createRGBSurface [SWSurface]
             width gameTop
             32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
  paintInfo hud resources (gameInfo shownState)
  SDL.blitSurface hud Nothing screen $ Just (SDL.Rect 0 0 (-1) (-1))

  -- The following line is BIG_ENDIAN specific
  -- The 32 is important because we are using Word32 for the masks
  -- FIXME: Should I use HWSurface and possibly other flags (alpha?)?
  surface <- createRGBSurface [SWSurface]
             gameWidth gameHeight
             32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
  paintMessage surface resources (gameStatus (gameInfo shownState))
  mapM_ (paintObject surface resources) $ gameObjects shownState
  SDL.blitSurface surface Nothing screen $ Just (SDL.Rect gameLeft gameTop (-1) (-1))

  -- Double buffering
  SDL.flip screen

paintInfo :: Surface -> Resources -> GameInfo -> IO ()
paintInfo screen resources over = void $ do
  -- Clear background
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 0x11 0x22 0x33
  fillRect screen Nothing bgColor

  -- Paint HUD
  message1 <- printSolid resources ("Level: " ++ show (gameLevel over))
  SDL.blitSurface message1 Nothing screen $ Just (SDL.Rect 10 10 (-1) (-1))

  message2 <- printSolid resources ("Points: " ++ show (gamePoints over))
  let h1 = SDL.surfaceGetHeight message1
  SDL.blitSurface message2 Nothing screen $ Just (SDL.Rect 10 (10 + h1 + 5) (-1) (-1))

  message3 <- printSolid resources ("Lives: " ++ show (gameLives over))
  renderAlignRight screen message3 (10, 10)

paintMessage :: Surface -> Resources -> GameStatus -> IO ()
paintMessage screen resources status =
    awhen (msg status) $ \msg' -> do
      message <- printSolid resources msg'
      renderAlignCenter screen message
  where
    msg GamePlaying     = Nothing
    msg GamePaused      = Just "Paused"
    msg (GameLoading n) = Just ("Level " ++ show n)
    msg GameOver        = Just "GAME OVER!!!"
    msg GameFinished    = Just "You won!!! Well done :)"

-- | Paints a game object on a surface.
paintObject :: Surface -> Resources -> Object -> IO ()
paintObject screen resources object =
  case objectKind object of
    Side -> return ()
    _    -> void $ SDL.blitSurface bI Nothing screen $ Just (SDL.Rect x y (-1) (-1))

  where
    (x, y) = both round $ objectTopLevelCorner object

    bI = imgSurface $ objectImg resources

    objectImg = case objectKind object of
      Paddle            -> paddleImg
      Block             -> let (BlockProps e pu _) = objectProperties object
                           in if pu then blockpuImg else blockImgF e
      Ball              -> ballImg
      PowerUp PointsUp  -> pointsUpImg
      PowerUp LivesUp   -> livesUpImg
      PowerUp NothingUp -> nothingUpImg
      PowerUp DestroyUp -> destroyUpImg

    blockImgF 3 = block1Img
    blockImgF 2 = block2Img
    blockImgF n = block3Img

-- * Auxiliary drawing functions

printSolid :: Resources -> String -> IO Surface
printSolid resources msg = do
  let font = unFont $ resFont resources
  message <- TTF.renderTextSolid font msg (SDL.Color 128 128 128)
  return message
