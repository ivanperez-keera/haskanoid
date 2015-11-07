{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP                       #-}
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

-- External imports
import Data.IORef
import Graphics.UI.SDL as SDL
import Control.Monad

-- External imports (Wiimote)
#ifdef wiimote
import Control.Monad(void)
import Control.Monad.IfElse (awhen)
import Data.Maybe (fromMaybe)
import System.CWiid
#endif

-- External imports (Kinect)
#ifdef kinect
import Control.Concurrent
import Data.Maybe (fromJust)
import Data.Vector.Storable (Vector,(!))
import Data.Word
import Freenect
import qualified Data.Vector.Storable as V
#endif

-- Internal imports
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
  let baseDev = sdlGetController

-- Fall back to mouse/kb is no kinect is present
#ifdef kinect
  print "Kinecting"
  dev <- do kn <- kinectController
            case kn of
              Nothing  -> return baseDev
              Just kn' -> return kn'
#else
  let dev = baseDev
#endif

-- Fall back to kinect or mouse/kb is no wiimote is present
#ifdef wiimote
  dev' <- do wm <- wiimoteDev
             return $ fromMaybe dev wm
#else
  let dev' = dev
#endif

  nr <- newIORef defaultInfo
  return $ ControllerRef (nr, dev')
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
#ifdef wiimote

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
   Nothing -> return Nothing
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
senseWiimote wmdev controller = do
  flags <- cwiidGetBtnState wmdev
  irs   <- cwiidGetIR wmdev

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
#endif

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
    MouseMotion x y _ _                      -> c { controllerPos   = (fromIntegral x, fromIntegral y)}
    MouseButtonDown _ _ ButtonLeft           -> c { controllerClick = True }
    MouseButtonUp   _ _ ButtonLeft           -> c { controllerClick = False} 
    KeyUp (Keysym { symKey = SDLK_p })       -> c { controllerPause = not (controllerPause c) }
    KeyDown (Keysym { symKey = SDLK_SPACE }) -> c { controllerClick = True  }
    KeyUp (Keysym { symKey = SDLK_SPACE })   -> c { controllerClick = False }
    _                                        -> c


-- Kinect

#ifdef kinect
kinectController :: ControllerDev
kinectController = do
  kref <- initializeKinect (gameWidth, gameHeight)
  return $ Just $ kinectGetController kref

kinectGetController :: KinectPosRef -> Controller -> IO Controller
kinectGetController kinectPosRef c = do
  kinectPos  <- readIORef kinectPosRef
  c' <- sdlGetController c
  let c'' = maybe c' (\p -> c' { controllerPos = p }) kinectPos
  return c''

-- TODO Use these instead of hard-coded values
kinectWidth, kinectHeight :: Int
kinectWidth  = 640
kinectHeight = 480

type KinectPosRef = IORef KinectPos
type KinectPos = Maybe (Double, Double)

initializeKinect :: (Double, Double) -> IO KinectPosRef
initializeKinect screenSize = do
  lastPos <- newIORef Nothing
  _ <- getDepthThread screenSize lastPos
  return lastPos

getDepthThread :: (Double, Double) -> KinectPosRef -> IO ThreadId
getDepthThread screenSize lastPos = forkIO $ do
  withContext $ \context -> do
    setLogLevel LogFatal context
    selectSubdevices context devices
    withDevice context index $ \device -> do
      setDepthMode device Medium ElevenBit
      setDepthCallback device $ \payload _timestamp -> do
        maybe (print ".") -- Too far or too close
              (updatePos lastPos)
              (calculateMousePos screenSize payload)
        return ()
      startDepth device
      forever $ processEvents context

  where devices = [Camera]
        index = 0 :: Integer

updatePos :: IORef (Maybe (Double, Double)) -> (Double, Double) -> IO ()
updatePos lastPosRef newPos@(nx,ny) = do
  lastPosM <- readIORef lastPosRef
  let (mx, my) = case lastPosM of
                   Nothing        -> newPos
                   (Just (lx,ly)) -> (adjust 50 lx nx, adjust 50 ly ny)
  writeIORef lastPosRef (Just (mx, my))
  mx `seq` my `seq` return ()

calculateMousePos :: (Double, Double) -> Vector Word16 -> Maybe (Double, Double) 
calculateMousePos (width, height) payload =
  fmap g (findFirst payload)
  where g (px,py) = (mousex, mousey)
         where
           pointerx = fromIntegral (640 - px)
           pointery = fromIntegral py
           mousex   = pointerx -- pointerx * adjx
           mousey   = pointery -- pointery * adjy
           adjx     = width  / 630.0
           adjy     = height / 470.0

mat :: Vector Float
mat = V.generate 2048 (\i -> let v :: Float
                                 v = ((fromIntegral i/2048.0)^3)*6.0 in v * 6.0 * 256.0)

findFirst :: Vector Word16 -> Maybe (Int, Int)
findFirst vs = fmap (\v -> (v `mod` 640, v `div` 640)) i
 where i  = V.findIndex (\x -> mat!(fromIntegral x) < 512) vs

processPayload :: Vector Word16 -> [(Float, Int, Int)]
processPayload ps = [(pval, tx, ty) | i <- [0..640*480-1]
                                    , let pval = mat!(fromIntegral (ps!i))
                                    , pval < 300
                                    , let ty = i `div` 640
                                          tx = i `mod` 640
                                    ]

-- Drop the fst elem, calculate the avg of snd and trd over the whole list
avg :: [(Float, Int, Int)] -> (Int, Int)
avg ls = (sumx `div` l, sumy `div` l)
  where l = length ls
        (sumx, sumy) = foldr (\(_,x,y) (rx,ry) -> (x+rx,y+ry)) (0,0) ls

-- Update a value, with a max cap
adjust :: (Num a, Ord a) => a -> a -> a -> a
adjust maxD old new
  | abs (old - new) < maxD = new
  | old < new              = old + maxD
  | otherwise              = old - maxD

#endif

