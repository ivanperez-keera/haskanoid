import Control.Monad.IfElse
import FRP.Yampa as Yampa
import FRP.Yampa.Record

import Game
import Display
import Input
import Graphics.UI.Extra.SDL
import System.Environment

-- TODO: Use MaybeT or ErrorT to report errors
main :: IO ()
main = do
  args <- getArgs

  let reactimateF = case args of
                      ["--record",   fp] -> Just (fp,RecordWriteOnly)
                      ["--replay",   fp] -> Just (fp,RecordReadOnly)
                      ["--continue", fp] -> Just (fp,RecordReadWrite)
                      _                  -> Nothing

  initializeDisplay

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources

  awhen res $ \res' -> do
    reactimateRecord
               reactimateF
               (initGraphs >> senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- fmap milisecsToSecs $ senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> render res' (fst e) >> return (snd e))
               (wholeGame &&& arr controllerQuit)
 
