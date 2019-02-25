{-# LANGUAGE CPP #-}
-- | Specifications of app resources.
module Resource.Specs where

-- External imports
import Game.Resource.Spec (ColorSpec, FontSpec, ImageSpec, MusicSpec, SoundSpec)

-- * Game assets

-- ** Images
initialBG           :: ImageSpec
initialBG           = ("data/level0.png", Nothing)

ballImage           :: ImageSpec
ballImage           = ("data/ball-alpha.png", Nothing)

block1Image         :: ImageSpec
block1Image         = ("data/block1.png", Nothing)

block2Image         :: ImageSpec
block2Image         = ("data/block2.png", Nothing)

block3Image         :: ImageSpec
block3Image         = ("data/block3.png", Nothing)

blockPuImage        :: ImageSpec
blockPuImage        = ("data/blockP.png", Nothing)

paddleImage         :: ImageSpec
paddleImage         = ("data/paddleBluA.png", Nothing)

pointsUpImage       :: ImageSpec
pointsUpImage       = ("data/diamond-alpha.png", Nothing)

livesUpImage       :: ImageSpec
livesUpImage       = ("data/heart-alpha.png", Nothing)

mockUpImage        :: ImageSpec
mockUpImage        = ("data/spike-y.png", Nothing)

destroyBallUpImage :: ImageSpec
destroyBallUpImage = ("data/spike-y.png", Nothing)

-- ** Audio
backgroundMusic :: MusicSpec
backgroundMusic = "data/level0.mp3"

blockHitSFX :: SoundSpec
blockHitSFX = ("data/196106_aiwha_ding-cc-by.wav", 2000)

-- ** Fonts
gameFontSpec :: FontSpec
gameFontSpec = ("data/lacuna.ttf", 32)

-- ** Colors
fontColor :: ColorSpec
fontColor = (228, 228, 228, 255)

black :: ColorSpec
black = (0, 0, 0, 255)
