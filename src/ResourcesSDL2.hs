{-# LANGUAGE CPP #-}
module ResourcesSDL2 where

-- * Game assets
import Resources

-- ** Images
initialBG       :: ImageSpec
initialBG       = ("data/level0.png", Nothing)

ballImage       :: ImageSpec
#ifdef sdl
ballImage       = ("data/ball2.png", Just (0, 255, 0))
#elif sdl2
ballImage       = ("data/ball-alpha.png", Nothing)
#endif

block1Image     :: ImageSpec
block1Image     = ("data/block1.png", Nothing)

block2Image     :: ImageSpec
block2Image     = ("data/block2.png", Nothing)

block3Image     :: ImageSpec
block3Image     = ("data/block3.png", Nothing)

paddleImage     :: ImageSpec
#ifdef sdl
paddleImage     = ("data/paddleBlu.png", Just (0, 255, 0))
#elif sdl2
paddleImage     = ("data/paddleBluA.png", Nothing)
#endif

pointsUpImage    :: ImageSpec
#ifdef sdl
pointsUpImage    = ("data/diamond2.png", Just (0, 255, 0))
#elif sdl2
pointsUpImage    = ("data/diamond2-alpha.png", Nothing)
#endif

livesUpImage    :: ImageSpec
#ifdef sdl
livesUpImage    = ("data/heart.png", Just (0, 255, 0))
#elif sdl2
livesUpImage    = ("data/heart-alpha.png", Nothing)
#endif

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
