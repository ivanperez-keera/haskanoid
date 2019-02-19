{-# LANGUAGE CPP #-}

-- External imports
import Control.Applicative       ((<$>))
import Control.Exception.Extra   (catchAny)
import Control.Monad.IfElse
import FRP.Yampa                 as Yampa
import Game.Resource.Manager.Ref
import Game.Resource.Spec
import Playground                (Settings)
import Playground.SDL            (initGraphs, loadAllResources)

-- Internal imports
import Resource.Values (settings)
import Game.Logic
import Input
import Paths_haskanoid

#if defined(sdl) || defined(sdl2)
-- External imports
import Game.Clock

-- Internal imports
import Game.Display
#endif

#ifdef ghcjs
import DisplayGHCJS
import GHCJSNow
-- import Control.Concurrent
-- import System.Mem
#endif

-- TODO: Use MaybeT or ErrorT to report errors
main :: IO ()
main = (`catchAny` print) $ do

  initializeDisplay
  renderingCtx  <- initGraphs (settings :: Settings Int)
  adjustSDLsettings

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  resSpec       <- localizeResourceSpec getDataFileName gameResourceSpec
  res           <- loadResources resSpec

  awhen res $ \res' -> do
    -- TODO: The 'undefined' is the runtime context.
    let env = (res', undefined, renderingCtx)
    loadAllResources env
    reactimate (senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- milisecsToSecs <$> senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> render e env >> return False) -- GHCJS: (\_ e -> render res' e >> threadDelay 1000 >> return False)
               wholeGame
