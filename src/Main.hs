-- External imports
import Control.Applicative ((<$>))
import Control.Monad.IfElse
import FRP.Yampa as Yampa

-- Internal imports
import Game
import Display
import Input
import Graphics.UI.Extra.SDL

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
                  dtSecs <- milisecsToSecs <$> senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ (e, c) -> render res' e >> return (controllerQuit c))
               (wholeGame &&& arr id)

