-- | A layer of abstraction on top of SDL audio.
--
-- It plays audio soundfx asynchronously (in a new thread), which means that
-- programs must be compiled with the threaded Runtime System (ghc flag is
-- -threaded).
--
-- This module is 2010-2014 (c) Keera Studios, redistributed with permission.
module Audio
    (Music(..),
     Audio(..),
     initAudio,
     loadAudio,
     loadMusic,
     playMusic,
     playFile,
     stopMusic,
     musicPlaying) where

import Control.Monad
import Control.Concurrent
import qualified Graphics.UI.SDL.Mixer.General as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import qualified Graphics.UI.SDL.Mixer.Music as SDL.Mixer.Music
import qualified Graphics.UI.SDL.Mixer.Types as SDL.Mixer.Types
import qualified Graphics.UI.SDL.Mixer.Samples as SDL.Mixer.Samples

data Music = Music { musicName :: String, unMusic :: SDL.Mixer.Types.Music }
data Audio = Audio { audioName :: String, unAudio :: SDL.Mixer.Types.Chunk }

-- | Initialize the audio subsystem.
--
-- Audio quality and number of channels are fixed (16).
initAudio :: IO ()
initAudio = void $ do
  _result <- SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 4096
  SDL.Mixer.Channels.allocateChannels 16

-- | Load a music file, returning a 'Music' if loaded successfully.
loadMusic :: String -> IO (Maybe Music)
loadMusic fp = fmap (fmap (Music fp)) $ SDL.Mixer.Music.tryLoadMUS fp

-- | Play music in a loop at max volume.
playMusic :: Music -> IO ()
playMusic m = do
  SDL.Mixer.Music.setMusicVolume 100
  SDL.Mixer.Music.playMusic (unMusic m) (-1)

-- | Stop playing music
stopMusic :: IO ()
stopMusic = SDL.Mixer.Music.haltMusic

-- | Is music playing?
musicPlaying :: IO Bool
musicPlaying = SDL.Mixer.Music.playingMusic

-- | Load an audio file.
loadAudio :: String -> IO (Maybe Audio)
loadAudio fp = fmap (fmap (Audio fp)) $ SDL.Mixer.Samples.tryLoadWAV fp

-- | Play an audio file for the given number of seconds.
--
-- This function spawns a new OS thread. Remember to compile your program
-- with the threaded RTS.
playFile :: Audio -> Int -> IO ()
playFile wav t = void $ forkOS $ do 
  _v <- SDL.Mixer.Channels.playChannel (-1) (unAudio wav) 0
  threadDelay (t * 1000)
