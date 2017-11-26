module ResourcesSDL2 where

import Data.Word

-- * Game assets

-- ** Images
initialBG       :: ImageSpec
initialBG       = ("data/level0.png", Nothing)

ballImage       :: ImageSpec
ballImage       = ("data/ball-alpha.png", Nothing)

block1Image     :: ImageSpec
block1Image     = ("data/block1.png", Nothing)

block2Image     :: ImageSpec
block2Image     = ("data/block2.png", Nothing)

block3Image     :: ImageSpec
block3Image     = ("data/block3.png", Nothing)

paddleImage     :: ImageSpec
paddleImage     = ("data/paddleBluA.png", Nothing)

pointsUpImage    :: ImageSpec
pointsUpImage    = ("data/diamond2-alpha.png", Nothing)

livesUpImage    :: ImageSpec
livesUpImage    = ("data/heart-alpha.png", Nothing)

-- ** Fonts
gameFontSpec    :: FontSpec
gameFontSpec    = ("data/lacuna.ttf", 32)

fontColor       :: ColorSpec
fontColor       = (228, 228, 228, 255)

-- ** Audio
backgroundMusic :: MusicSpec
backgroundMusic = "data/level0.mp3"

blockHitSFX     :: SoundFXSpec
blockHitSFX     = ("data/196106_aiwha_ding-cc-by.wav", 2000)

type ImageSpec   = (FilePath, Maybe (Word8, Word8, Word8))
type MusicSpec   = FilePath
type SoundFXSpec = (FilePath, Int)
type FontSpec    = (FilePath, Int)
type ColorSpec   = (Int, Int, Int, Int)