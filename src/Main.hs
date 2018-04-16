{-# LANGUAGE CPP #-}
import Control.Applicative       ((<$>))
import Control.Exception.Extra   (catchAny)
import Control.Monad.IfElse
import FRP.Yampa                 as Yampa
import Game.Resource.Manager.Ref
import Game.Resource.Spec

import GamePlay
import Input
import Paths_haskanoid

#ifdef sdl
import Display
import Game.Clock
#endif

#ifdef sdl2
import Display
import Game.Clock
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

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  resSpec       <- localizeResourceSpec getDataFileName gameResourceSpec
  res           <- loadResources resSpec

  awhen res $ \res' -> do
    renderingCtx <- initGraphs res'
    reactimate (senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- milisecsToSecs <$> senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> render res' e renderingCtx >> return False) -- GHCJS: (\_ e -> render res' e >> threadDelay 1000 >> return False)
               wholeGame
