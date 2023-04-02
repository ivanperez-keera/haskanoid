-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Auxiliary functions to calculate frames-per-second.
module Graphics.UI.Extra.FPS where

-- External imports
import Control.Monad
import Data.IORef

type FPSCounter = (IO Int, IORef FPSCounter', Int)
type FPSCounter' = ( Int -- Last time
                   , Int
                   )

initialiseFPSCounter :: Int -> IO Int -> IO FPSCounter
initialiseFPSCounter every clock = do
  _ <- clock
  _ <- clock
  _ <- clock
  _ <- clock
  _ <- clock
  time <- clock
  ref <- newIORef (time, every)
  return (clock, ref, every)

stepFPSCounter :: FPSCounter -> IO ()
stepFPSCounter (clock, fpsRef, every) = do
  (lastTime, left) <- readIORef fpsRef
  let left' = left - 1
  if left' < 0
    then do newTime <- clock
            let
                msf = fromIntegral (newTime - lastTime)
                    / (fromIntegral every :: Double)

                fps = 1000 / msf
            do -- when (msf > 0) $
              putStrLn $ "Performance report :: Time per frame: " ++ show msf
                         ++ "ms, FPS: " ++ show fps
                         ++ ", Total running time: " ++ show newTime
            writeIORef fpsRef (newTime, every)
    else do writeIORef fpsRef (lastTime, left')
