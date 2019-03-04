{-# LANGUAGE CPP #-}
module Main where
-- External imports
-- import Control.Applicative       ((<$>))
import Control.Exception.Extra   (catchAny)
import Control.Monad.IfElse      (awhen)
import FRP.Yampa                 as Yampa (arr, reactimate, (&&&))
import Game.Resource.Manager.Ref (loadResources)
import Game.Resource.Spec        (localizeResourceSpec)
import Playground                (Settings)

-- Internal imports
import Game.Logic      (wholeGame)
import Resource.Values (getDataFileName, settings)
import UserInput       (Controller, initInputDevices, senseInput)

#if defined(sdl) || defined(sdl2)
-- External imports
import Game.Clock     (initializeTimeRef, milisecsToSecs, senseTimeRef)
import Playground.SDL (createRuntimeContext, initDeviceOutput, initGraphs,
                       loadAllResources)

-- Internal imports
import Game.DeviceOutput (adjustSDLsettings, render)
import Resource.Manager  (gameResourceSpec)
#endif

#ifdef ghcjs
import DisplayGHCJS
import GHCJSNow
-- import Control.Concurrent
-- import System.Mem
#endif

-- | Start the game.
main :: IO ()
main = my_main_main

-- | Start the game and keep the game loop alive.
my_main_main :: IO ()
my_main_main =
  -- Print exceptions
  (`catchAny` print) $ do

    -- Initialize output subsystems (video, audio).
    initDeviceOutput
    rCtx    <- initGraphs (settings :: Settings Int)
    adjustSDLsettings

    -- Initialize and prepare clock.
    timeRef <- initializeTimeRef

    -- Initialize and prepare input, resources and output subsystems.
    ctrlRef <- initInputDevices
    resSpec <- localizeResourceSpec getDataFileName gameResourceSpec
    rMgr    <- loadResources resSpec
    rtCtx   <- createRuntimeContext rCtx

    awhen rMgr $ \rMgr' -> do
      let env = (rMgr', rtCtx, rCtx)
      loadAllResources env

      reactimate
        (senseInput ctrlRef)
        (\_ -> do
           -- Get clock
           dtSecs <- milisecsToSecs <$> senseTimeRef timeRef
           -- Get new input
           ctrl <- senseInput ctrlRef
           return (dtSecs, Just ctrl)
        )
        (\_ e -> render e env >> return False) -- GHCJS: (\_ e -> render rMgr' e >> threadDelay 1000 >> return False)
        wholeGame
