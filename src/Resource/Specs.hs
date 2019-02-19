{-# LANGUAGE CPP #-}
module Resource.Specs where

-- External imports
import Game.Resource.Spec

-- * Game assets

-- ** Images
initialBG           :: ImageSpec
initialBG           = ("data/level0.png", Nothing)

ballImage           :: ImageSpec
#ifdef sdl
ballImage           = ("data/ball2.png", Just (0, 255, 0))
#elif sdl2
ballImage           = ("data/ball-alpha.png", Nothing)
#endif

block1Image         :: ImageSpec
block1Image         = ("data/block1.png", Nothing)

block2Image         :: ImageSpec
block2Image         = ("data/block2.png", Nothing)

block3Image         :: ImageSpec
block3Image         = ("data/block3.png", Nothing)

blockPuImage        :: ImageSpec
blockPuImage        = ("data/blockP.png", Nothing)

paddleImage         :: ImageSpec
#ifdef sdl
paddleImage         = ("data/paddleBlu.png", Just (0, 255, 0))
#elif sdl2
paddleImage         = ("data/paddleBluA.png", Nothing)
#endif

pointsUpImage       :: ImageSpec
#ifdef sdl
pointsUpImage       = ("data/diamond2.png", Just (0, 255, 0))
#elif sdl2
pointsUpImage       = ("data/diamond-alpha.png", Nothing)
#endif

livesUpImage       :: ImageSpec
#ifdef sdl
livesUpImage       = ("data/heart.png", Just (0, 255, 0))
#elif sdl2
livesUpImage       = ("data/heart-alpha.png", Nothing)
#endif

mockUpImage        :: ImageSpec
#ifdef sdl
mockUpImage        = ("data/spike-y.png", Just (0, 255, 0))
#elif sdl2
mockUpImage        = ("data/spike-y.png", Nothing)
#endif

destroyBallUpImage :: ImageSpec
#ifdef sdl
destroyBallUpImage = ("data/spike-b.png", Just (0, 255, 0))
#elif sdl2
destroyBallUpImage = ("data/spike-y.png", Nothing)
#endif

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
