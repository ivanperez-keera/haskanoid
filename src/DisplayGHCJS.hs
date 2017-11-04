module DisplayGHCJS where

import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Data.String (fromString)
import           Data.Coerce
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
import qualified JavaScript.Web.Canvas         as C
import qualified JavaScript.Web.Canvas.Internal as C
import           JsImports                     (now)


import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                 hiding (sequence_)
import           Data.Foldable                 (minimumBy)
import           Data.Ord
import           Data.Semigroup
import           Linear


import Constants
import GameState
import Objects
import Resources hiding (audio)
import Levels
import Paths_haskanoid

-- | Ad-hoc resource loading
-- This function is ad-hoc in two senses: first, because it
-- has the paths to the files hard-coded inside. And second,
-- because it loads the specific resources that are needed,
-- not a general 
--
loadResources :: IO (Maybe ResourceMgr)
loadResources = runMaybeT $ do
  ---- Font initialization
  --ttfOk <- lift TTF.init
  --
  --gameFont <- liftIO $ getDataFileName "data/lacuna.ttf"
  ---- Load the fonts we need
  --font  <- liftIO $ TTF.tryOpenFont gameFont 32 -- What does the 32 do?
  --let myFont = fmap (Font gameFont) font

  --blockHit <- liftIO $ loadAudio =<< getDataFileName "data/196106_aiwha_ding-cc-by.wav"

  ---- bgM <- liftIO $ loadMusic "Ckotty_-_Game_Loop_11.ogg"
  ---- bgM <- liftIO $ loadMusic "data/level0.mp3"

  ---- let levelBg = "data/level0.png"
  ---- img <- lift $ fmap (Image levelBg) $ load levelBg

  --ballImg <- liftIO $ getDataFileName "data/ball2.png"
  --ball <- lift $ fmap (Image ballImg) $ load ballImg

  --b1Img <- liftIO $ getDataFileName "data/block1.png"
  --b1 <- lift $ fmap (Image b1Img) $ load b1Img

  --b2Img <- liftIO $ getDataFileName "data/block2.png"
  --b2 <- lift $ fmap (Image b2Img) $ load b2Img

  --b3Img <- liftIO $ getDataFileName "data/block3.png"
  --b3 <- lift $ fmap (Image b3Img) $ load b3Img

  --paddleImg <- liftIO $ getDataFileName "data/paddleBlu.png"
  --paddle <- lift $ fmap (Image paddleImg) $ load paddleImg

  ---- Start playing music
  ---- when (isJust bgM) $ lift (playMusic (fromJust bgM))

  ---- Return Nothing or embed in Resources
  --res <- case (myFont, blockHit) of
  --         (Just f, Just b) -> let 
  --                             in return (Resources f b Nothing ball b1 b2 b3 paddle Nothing)
  --         _                        -> do liftIO $ putStrLn "Some resources could not be loaded"
  --                                        mzero

  liftIO $ fmap ResourceMgr $
    newIORef (ResourceManager (GameStarted) Resources)

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
  -- -- Create window
  -- screen <- SDL.setVideoMode (round width) (round height) 32 [SWSurface]
  -- SDL.setCaption "Test" ""

  -- -- Important if we want the keyboard to work right (I don't know
  -- -- how to make it work otherwise)
  -- SDL.enableUnicode True

  -- -- Hide mouse
  -- SDL.showCursor False


render :: ResourceMgr -> GameState -> IO()
render resourceManager shownState = do
  -- resources <- loadNewResources resourceManager shownState
  let resources = Resources
  audio   resources shownState
  display resources shownState

audio :: Resources -> GameState -> IO()
audio resources shownState = do
  return ()
  -- Start bg music if necessary
  -- playing <- musicPlaying
  -- unless playing $ awhen (bgMusic resources) playMusic 

  -- -- Play object hits
  -- mapM_ (audioObject resources) $ gameObjects shownState

-- audioObject resources object = when (objectHit object) $
--   case objectKind object of
--     (Block _ _) -> playFile (blockHitSnd resources) 3000
--     _           -> return ()

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

paintGeneralMsg screen resources GamePlaying     = return ()
paintGeneralMsg screen resources GamePaused      = paintGeneralMsg' screen resources "Paused"
paintGeneralMsg screen resources (GameLoading n) = paintGeneralMsg' screen resources ("Level " ++ show n)
paintGeneralMsg screen resources GameOver        = paintGeneralMsg' screen resources "GAME OVER!!!"
paintGeneralMsg screen resources GameFinished    = paintGeneralMsg' screen resources "You won!!! Well done :)"

paintGeneralMsg' screen resources msg = void $ do
  C.fillStyle 94 65 47 1 screen
  C.font (fromString "34px Arial") screen
  C.textBaseline C.Top screen
  C.textAlign C.Center screen
  C.fillText (fromString msg) (width / 2) (height / 2) screen

paintGeneral screen resources over = void $ do
  -- Paint background
  C.fillStyle 94 65 47 1 screen
  C.fillRect 0 0 width gameTop screen
  -- Paint HUG
  paintGeneralHUD screen resources over

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
    _               -> return ()
  where p = objectPos object
        x = bx + fst p
        y = by + snd p

newtype ResourceMgr = ResourceMgr { unResMgr :: IORef ResourceManager }

data ResourceManager = ResourceManager
  { lastKnownStatus :: GameStatus
  , resources       :: Resources
  }

data Resources = Resources
  -- { resFont     :: Font
  -- , blockHitSnd :: Audio
  -- , bgImage     :: Maybe Image
  -- , ballImg     :: Image
  -- , block1Img   :: Image
  -- , block2Img   :: Image
  -- , block3Img   :: Image
  -- , paddleImg   :: Image
  -- , bgMusic     :: Maybe Music
  -- }

getContext :: Element -> IO C.Context
getContext = C.getContext . coerce


-- data Image = Image { imgName  :: String, imgSurface :: Surface }
-- data Font  = Font  { fontName :: String, unFont :: TTF.Font }

--loadNewResources :: ResourceMgr ->  GameState -> IO Resources
--loadNewResources mgr state = do
--  manager <- readIORef (unResMgr mgr)
--  let oldState = lastKnownStatus manager
--      newState = gameStatus (gameInfo state)
--      oldResources = resources manager
--
--  newResources <- case newState of
--                    (GameLoading _) | (newState /= oldState)
--                                    -> updateAllResources oldResources newState
--                    _               -> return oldResources 
--
--  let manager' = ResourceManager { lastKnownStatus = newState
--                                 , resources       = newResources
--                                 }
--
--  writeIORef (unResMgr mgr) manager'
--  return newResources

-- updateAllResources :: Resources -> GameStatus -> IO Resources
-- updateAllResources res (GameLoading n) = do
--   -- Load new music
--   let newMusicFP' = _resourceFP $ levelMusic $ levels !! n
--   newMusicFP <- getDataFileName newMusicFP'
-- 
--   let oldMusic   = bgMusic res
--       oldMusicFP = maybe "" musicName oldMusic
-- 
--   newMusic <- if (oldMusicFP == newMusicFP)
--               then return oldMusic
--               else do -- Loading can fail, in which case we continue
--                       -- with the old music
--                       bgM <- loadMusic newMusicFP
--                       if isNothing bgM
--                        then do putStrLn $ "Could not load resource " ++ newMusicFP
--                                return oldMusic
--                        else do stopMusic
--                                return bgM
-- 
--   -- Load new background
--   let newBgFP' = _resourceFP $ levelBg $ levels !! n
-- 
--   newBgFP <- getDataFileName newBgFP'
-- 
--   let oldBg   = bgImage res
--       oldBgFP = maybe "" imgName oldBg
-- 
--   newBg <- if oldBgFP == newBgFP
--              then return oldBg
--              else do img' <- load newBgFP
--                      return $ Just (Image newBgFP img')
-- 
--   return (res { bgImage = newBg, bgMusic = newMusic })
