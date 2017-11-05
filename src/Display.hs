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
import Graphics.UI.SDL              as SDL
import Graphics.UI.Extra.SDLDrawing as SDL
import Graphics.UI.SDL.TTF          as TTF

import Constants
import GameState
import Objects
import Resources
import ResourceManager

-- * Initialization

initializeDisplay :: IO ()
initializeDisplay = do
   -- Initialise SDL
  SDL.init [InitEverything]

  initAudio

initGraphs :: IO ()
initGraphs = do
  -- Create window
  screen <- SDL.setVideoMode (round width) (round height) 32 [SWSurface]
  SDL.setCaption "Test" ""

  -- Important if we want the keyboard to work right (I don't know
  -- how to make it work otherwise)
  SDL.enableUnicode True

  -- Hide mouse
  SDL.showCursor False

-- * Rendering and Sound

-- | Loads new resources, renders the game state using SDL, and adjusts music.
render :: ResourceMgr -> GameState -> IO()
render resourceManager shownState = do
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
    (Block _ _) -> playFile (blockHitSnd resources) 3000
    _           -> return ()

-- ** Painting

display :: Resources -> GameState -> IO()
display resources shownState = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen

  -- Paint background

  awhen (bgImage resources) $ \bg' -> void $ do
    let bg     = imgSurface bg'
    let rectBg = SDL.Rect 0 0 (round width) (round height)
    SDL.blitSurface bg Nothing screen $ Just rectBg

  hud <- createRGBSurface [SWSurface]
             (round width) (round gameTop)
             32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
  paintGeneral hud resources (gameInfo shownState)
  let rectHud = SDL.Rect 0 0 (round width) (round gameTop)
  SDL.blitSurface hud Nothing screen $ Just rectHud

  -- The following line is BIG_ENDIAN specific
  -- The 32 is important because we are using Word32 for the masks
  -- FIXME: Should I use HWSurface and possibly other flags (alpha?)?
  surface <- createRGBSurface [SWSurface]
             (round gameWidth) (round gameHeight)
             32 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
  paintGeneralMsg surface resources (gameStatus (gameInfo shownState))
  mapM_ (paintObject resources surface) $ gameObjects shownState
  let rect = SDL.Rect (round gameLeft) (round gameTop) (round gameWidth) (round gameHeight)
  SDL.blitSurface surface Nothing screen $ Just rect

  -- Double buffering
  SDL.flip screen

paintGeneral :: Surface -> Resources -> GameInfo -> IO ()
paintGeneral screen resources over = void $ do
  -- Paint background
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 0x11 0x22 0x33
  fillRect screen Nothing bgColor
  -- Paint HUD
  paintGeneralHUD screen resources over

paintGeneralMsg :: Surface -> Resources -> GameStatus -> IO ()
paintGeneralMsg screen resources GamePlaying     = return ()
paintGeneralMsg screen resources GamePaused      = paintGeneralMsg' screen resources "Paused"
paintGeneralMsg screen resources (GameLoading n) = paintGeneralMsg' screen resources ("Level " ++ show n)
paintGeneralMsg screen resources GameOver        = paintGeneralMsg' screen resources "GAME OVER!!!"
paintGeneralMsg screen resources GameFinished    = paintGeneralMsg' screen resources "You won!!! Well done :)"

paintGeneralMsg' :: Surface -> Resources -> String -> IO ()
paintGeneralMsg' screen resources msg = void $ do
  message <- printSolid resources msg
  renderAlignCenter screen message

paintGeneralHUD :: Surface -> Resources -> GameInfo -> IO ()
paintGeneralHUD screen resources over = void $ do

  message1 <- printSolid resources ("Level: " ++ show (gameLevel over))
  SDL.blitSurface message1 Nothing screen $ Just (SDL.Rect 10 10 (-1) (-1))

  message2 <- printSolid resources ("Points: " ++ show (gamePoints over))
  let h2 = SDL.surfaceGetHeight message2
  SDL.blitSurface message2 Nothing screen $ Just (SDL.Rect 10 (10 + h2 + 5) (-1) (-1))

  message3 <- printSolid resources ("Lives: " ++ show (gameLives over)) 
  renderAlignRight screen message3 (10, 10)

-- * Auxiliary drawing functions
printSolid :: Resources -> String -> IO Surface
printSolid resources msg = do
  let font = unFont $ resFont resources
  message <- TTF.renderTextSolid font msg (SDL.Color 128 128 128)
  return message

-- | Paints a game object on a surface.
paintObject :: Resources -> Surface -> Object -> IO ()
paintObject resources screen object =
  case objectKind object of
    (Paddle _ ) -> void $ do let bI = imgSurface $ paddleImg resources
                             SDL.blitSurface bI Nothing screen $ Just (SDL.Rect x y (-1) (-1))

    (Block e _) -> void $ do let bI = imgSurface $ blockImage e
                             SDL.blitSurface bI Nothing screen $ Just (SDL.Rect x y (-1) (-1))

    (Ball r)    -> void $ do let (x', y') = (x - round r, y - round r)

                             let bI = imgSurface $ ballImg resources
                             SDL.blitSurface bI Nothing screen $ Just (SDL.Rect x' y' (-1) (-1))
    _           -> return ()
  where (x, y) = both round (objectPos object)
        blockImage 3 = block1Img resources
        blockImage 2 = block2Img resources
        blockImage n = block3Img resources
