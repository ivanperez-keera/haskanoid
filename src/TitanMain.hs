{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE Arrows                      #-}
{-# LANGUAGE ForeignFunctionInterface    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Control.Monad.IfElse
import FRP.Yampa as Yampa

import Game
import GameState (GameState)
import Display
import Input
import Graphics.UI.Extra.SDL
import FRP.Yampa       as Yampa
import Data.IORef
import FRP.Titan.Debug.CommTCP
import FRP.Titan.Debug.Yampa

-- TODO: Use MaybeT or ErrorT to report errors
main :: IO ()
main = do

  initializeDisplay

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources
  bridge <- mkTitanCommTCPBridge

  awhen res $ \res' -> do
    reactimateControl
             bridge                         -- Communication channels
             defaultPreferences             -- Simulation preferences
             ([] :: [Command GamePred]) -- Initial command queue
               (initGraphs >> senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- fmap milisecsToSecs $ senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> render res' e >> return False)
               wholeGame

-- | Nothing to observe here
data GamePred = GamePred
 deriving (Read, Show)

instance Pred GamePred Controller GameState where
  evalPred p dt i ey = True
