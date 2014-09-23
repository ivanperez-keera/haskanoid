{-# LANGUAGE ExistentialQuantification #-}
-- | Defines an abstraction for the game controller and the functions to read
-- it.
--
-- Lower-level devices replicate the higher-level API, and should accomodate to
-- it. Each device should:
--
--    - Upon initialisation, return any necessary information to poll it again.
--
--    - Update the controller with its own values upon sensing.
--
-- In this case, we only have two: a wiimote and a mouse/keyboard combination.
-- If the wiimote is available, then the mouse isn't used.
--
-- It's very easy to add new devices. In particular, adding a Kinect using the
-- freenect library is easy (see github.com/keera-studios/freenect) for an
-- updated version.
--
-- Limitations:
-- 
--    - Device failures are not handled.
--
--    - Falling back to the next available device when there's a problem.
--
--    - Keymap configuration (having an intermediate Action-based layer).
--
--    - Using more than one device at a time. Changing that would be a one-line
--    patch.
--
module Input where

import Control.Monad
import Control.Monad.IfElse
import Data.IORef
import Data.Maybe (fromJust)
import Graphics.UI.SDL as SDL
import System.CWiid

import Control.Extra.Monad
import Graphics.UI.Extra.SDL

import Constants

-- * Game controller

-- | Controller info at any given point.
data Controller = Controller
  { controllerPos   :: (Double, Double)
  , controllerClick :: Bool
  , controllerPause :: Bool
  }

-- | Controller info at any given point, plus a pointer
-- to poll the main device again. This is safe,
-- since there is only one writer at a time (the device itself).
newtype ControllerRef =
  ControllerRef (IORef Controller, Controller -> IO Controller)

-- * General API

-- | Initialize the available input devices. This operation
-- returns a reference to a controller, which enables
-- getting its state as many times as necessary. It does
-- not provide any information about its nature, abilities, etc.
initializeInputDevices :: IO ControllerRef
initializeInputDevices = do
  dev <- do wm <- wiimoteDev
            case wm of
              Nothing  -> fmap fromJust sdlMouseKB
              Just wm' -> return wm'
  nr <- newIORef defaultInfo
  return $ ControllerRef (nr, dev)
 where defaultInfo = Controller (0,0) False False

-- | Sense from the controller, providing its current
-- state. This should return a new Controller state
-- if available, or the last one there was.
-- 
-- It is assumed that the sensing function is always
-- callable, and that it knows how to update the
-- Controller info if necessary.
senseInput :: ControllerRef -> IO Controller
senseInput (ControllerRef (cref, sensor)) = do
  cinfo <- readIORef cref
  cinfo' <- sensor cinfo
  writeIORef cref cinfo'
  return cinfo'

type ControllerDev = IO (Maybe (Controller -> IO Controller))

-- * WiiMote API (mid-level)

-- | The wiimote controller as defined using this
-- abstract interface. See 'initializeWiimote'.
wiimoteDev :: ControllerDev
wiimoteDev = initializeWiimote

-- ** Initialisation

-- | Initializes the wiimote, optionally returning the sensing function. It
-- returns Nothing if the Wiimote cannot be detected. Users should have a BT
-- device and press 1+2 to connect to it. A message is shown on stdout.
initializeWiimote :: ControllerDev
initializeWiimote = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  awhen wm (void . (`cwiidSetRptMode` 15)) -- Enable button reception, acc and IR
  case wm of
    Nothing  -> return Nothing
    Just wm' -> return $ Just $ senseWiimote wm'

-- ** Sensing

-- | Sense the Wiimote and update the controller.
--
-- This operation uses the IR for the controller's position,
-- and the main (A) button for the click.
--
-- TODO: Allow configuring the button and using other motion mechamisms
-- (accelerometers).
--
-- TODO: This should be split in two operations. One that presents a nice
-- Wii-like interface and one that actually updates the controller
senseWiimote :: CWiidWiimote -> Controller -> IO Controller
senseWiimote wiimote controller = do
  flags <- cwiidGetBtnState wiimote
  irs   <- cwiidGetIR wiimote

  -- Obtain positions of leds 1 and 2 (with a normal wii bar, those
  -- will be the ones we use).
  let led1   = irs!!0
      led2   = irs!!1

  -- Calculate mid point between sensor bar leds
  let posX = ((cwiidIRSrcPosX led1) + (cwiidIRSrcPosX led2)) `div` 2
      posY = ((cwiidIRSrcPosY led1) + (cwiidIRSrcPosY led2)) `div` 2

  -- Calculate proportional coordinates
  let propX = fromIntegral (1024 - posX) / 1024.0
      propY = fromIntegral (max 0 (posY - 384)) / 384.0

  -- Calculate game area coordinates
  let finX  = width  * propX
      finY  = height * propY

  -- Direction (old system based on buttons)
  -- let isLeft  = cwiidIsBtnPushed flags cwiidBtnLeft
  --     isRight = cwiidIsBtnPushed flags cwiidBtnRight 
  --     (x,y)   = controllerPos controller
  --     x'      = if isLeft then x - wiiXDiff else if isRight then x + wiiXDiff else x
  --     x''     = inRange (0, gameWidth) x' 
  --     pos'    = (x'', y)
  -- wiiXDiff :: Float
  -- wiiXDiff = 6

  -- Clicks
  let isClick = cwiidIsBtnPushed flags cwiidBtnA

  -- Update state
  return (controller { controllerPos   = (finX, finY) -- pos'
                     , controllerClick = isClick
                     })

-- * SDL API (mid-level)

-- ** Initialization

-- | Dummy initialization. No device is actually initialized.
sdlMouseKB :: ControllerDev
sdlMouseKB = return (Just sdlGetController)

-- ** Sensing

-- | Sense the SDL keyboard and mouse and update
-- the controller. It only senses the mouse position,
-- the primary mouse button, and the p key to pause
-- the game.
--
-- We need a non-blocking controller-polling function.
-- TODO: Check http://gameprogrammer.com/fastevents/fastevents1.html
sdlGetController :: Controller -> IO Controller
sdlGetController info =
  foldLoopM info pollEvent (not.isEmptyEvent) ((return .) . handleEvent)

-- | Handles one event only and returns the updated controller.
handleEvent :: Controller -> SDL.Event -> Controller
handleEvent c e =
  case e of
    MouseMotion x y _ _                -> c { controllerPos   = (fromIntegral x, fromIntegral y)}
    MouseButtonDown _ _ ButtonLeft     -> c { controllerClick = True }
    MouseButtonUp   _ _ ButtonLeft     -> c { controllerClick = False} 
    KeyUp (Keysym { symKey = SDLK_p }) -> c { controllerPause = not (controllerPause c) }
    _                                  -> c
