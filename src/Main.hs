{-# LANGUAGE CPP #-}
import Control.Monad.IfElse
import FRP.Yampa as Yampa
import Control.Concurrent

import Game
import Display
import Input
#ifdef sdl
import Graphics.UI.Extra.SDL
#elif ghcjs
import GHCJSNow
#endif

-- TODO: Use MaybeT or ErrorT to report errors
main :: IO ()
main = do

  initializeDisplay

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources

  awhen res $ \res' -> do
    reactimate (initGraphs >> senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- fmap milisecsToSecs $ senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> render res' e >> threadDelay 1000 >> return False)
               wholeGame
 
