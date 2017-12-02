module DisplayGHCJS where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad                 hiding (sequence_)
import           Control.Monad.IfElse
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Coerce
import           Data.Foldable                 (minimumBy)
import           Data.IORef
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String (fromString)
import           GHCJS.Concurrent              ( synchronously )
import           GHCJS.DOM                     ( currentDocument
                                               , currentWindow )
import           GHCJS.DOM.Document            ( getBody
                                               , getElementById )
import           GHCJS.DOM.Element             ( getOffsetLeft
                                               , getOffsetTop
                                               , getInnerHTML )
import           GHCJS.DOM.Element             ( setInnerHTML )
import           GHCJS.DOM.EventTarget         ( addEventListener )
import           GHCJS.DOM.EventTargetClosures ( eventListenerNew )
import           GHCJS.DOM.Types               ( Element(..), IsDocument
                                               , MouseEvent, unElement )
import           GHCJS.DOM.UIEvent             ( getPageX, getPageY )
import           GHCJS.Foreign
import           GHCJS.Types
import qualified JavaScript.Web.Canvas          as C
import qualified JavaScript.Web.Canvas.Internal as C

import           Linear


import Constants
import GameState
import Objects
import Resources hiding (audio)
import Levels
import Paths_haskanoid

-- * Initialization

initializeDisplay :: IO ()
initializeDisplay = do
  -- get Canvas context
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body (Just initialHtml)

initialHtml :: String
initialHtml = "<canvas id=\"dia\" width=\"" ++ show (round width)
              ++ "\" height=\"" ++ show (round height)
              ++ "\" style=\"border: 1px solid\"></canvas>"

initGraphs :: IO ()
initGraphs = do
  return ()

-- * Rendering and Sound

-- | Loads new resources, renders the game state, and adjusts music.
render :: ResourceMgr -> GameState -> IO()
render resourceManager shownState = do
  -- resources <- loadNewResources resourceManager shownState
  let resources = Resources
  audio   resources shownState
  display resources shownState

-- ** Audio

audio :: Resources -> GameState -> IO()
audio resources shownState = do
  -- Start bg music if necessary
  -- playing <- musicPlaying
  -- unless playing $ awhen (bgMusic resources) playMusic

  -- Play object hits
  mapM_ (audioObject resources) $ gameObjects shownState

audioObject resources object = return ()

display :: Resources -> GameState -> IO()
display resources shownState = synchronously $ do
  -- Obtain surface
  Just doc <- currentDocument
  Just canvas <- getElementById doc "dia"
  ctx <- getContext canvas

  -- Paint background
  C.fillStyle 252 235 182 1.0 ctx
  C.fillRect 0 0 width height ctx

  mapM_ (paintObject (gameLeft, gameTop) resources ctx) $ gameObjects shownState

  -- HUD
  paintGeneral ctx resources (gameInfo shownState)
  paintGeneralMsg ctx resources (gameStatus (gameInfo shownState))

  -- Double buffering
  -- C.fill ctx

paintGeneral :: Surface -> Resources -> GameInfo -> IO ()
paintGeneral screen resources over = void $ do
  -- Paint background
  C.fillStyle 94 65 47 1 screen
  C.fillRect 0 0 width gameTop screen
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
  C.fillStyle 94 65 47 1 screen
  C.font (fromString "34px Arial") screen
  C.textBaseline C.Top screen
  C.textAlign C.Center screen
  C.fillText (fromString msg) (width / 2) (height / 2) screen


paintGeneralHUD :: Surface -> Resources -> GameInfo -> IO ()
paintGeneralHUD screen resources over = void $ do
  C.fillStyle 252 235 182 1.0 screen
  C.font (fromString "34px Arial") screen
  C.textBaseline C.Top screen
  C.textAlign C.Left screen
  C.fillText (fromString $ "Level: " ++ show (gameLevel over)) 10 10 screen
  C.fillText (fromString $ "Points: " ++ show (gamePoints over)) 10 50 screen
  C.textAlign C.Right screen
  C.fillText (fromString $ "Lives: " ++ show (gameLives over)) (width-10) 10 screen

paintObject (bx, by) resources screen object = do
  case objectKind object of
    (Paddle (w,h))  -> void $ do C.fillStyle 120 192 168 1.0 screen
                                 C.fillRect x y w h screen
    (Block e (w,h)) -> void $ do case e of
                                   3 -> C.fillStyle 240 120 24 1.0 screen
                                   2 -> C.fillStyle 220 108 21 1.0 screen
                                   n -> C.fillStyle 200  99 19 1.0 screen
                                 C.fillRect x y w h screen
    (Ball r)        -> void $ do C.beginPath screen
                                 C.arc x y r 0 (2*pi) False screen
                                 C.fillStyle 240 168 48 1.0 screen
                                 C.fill screen
    -- TODO: Check that the drawing is ok, possibly subsitute for a
    -- proper diamond.
    (PDiamond (w,h)) -> void $ do C.fillStyle 240 168 48 1.0 screen
                                  C.fillRect x y w h screen
                                  C.beginPath screen
                                  C.arc x y r 0 (2*pi) False screen
                                  C.fillStyle 220 108 21 1.0 screen
                                  C.fill screen
    _               -> return ()
  where p = objectPos object
        x = bx + fst p
        y = by + snd p

getContext :: Element -> IO C.Context
getContext = C.getContext . coerce

-- * Resource management

newtype ResourceMgr = ResourceMgr { unResMgr :: IORef ResourceManager }

data ResourceManager = ResourceManager
  { lastKnownStatus :: GameStatus
  , resources       :: Resources
  }

-- | Includes all the assets needed at the current time in the game.
data Resources = Resources

-- | Ad-hoc resource loading
loadResources :: IO (Maybe ResourceMgr)
loadResources = runMaybeT $
  liftIO $ ResourceMgr <$>
    newIORef (ResourceManager GameStarted Resources)
