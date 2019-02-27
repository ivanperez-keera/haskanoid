{-# LANGUAGE CPP #-}
-- | Specifications of app resources.
module Resource.Specs where

-- External imports
import Game.Resource.Spec (ColorSpec, FontSpec, ImageSpec, MusicSpec, SoundSpec)

-- * Source path

basePath :: FilePath
basePath = "data/"

-- * Images

imgPath :: FilePath
imgPath = basePath ++ "images/"

-- ** Screens

screensPath :: FilePath
screensPath = imgPath ++ "screens/"

initialBG           :: ImageSpec
initialBG           = (screensPath ++ "level0.png", Nothing)

-- ** Icons
-- *** Game specific resources

gameSpecificPath :: FilePath
gameSpecificPath = imgPath ++ "res-game/"

ballImage :: ImageSpec
ballImage = (gameSpecificPath ++ "ball-alpha.png", Nothing)

block1Image :: ImageSpec
block1Image = (gameSpecificPath ++ "block1.png", Nothing)

block2Image :: ImageSpec
block2Image = (gameSpecificPath ++ "block2.png", Nothing)

block3Image :: ImageSpec
block3Image = (gameSpecificPath ++ "block3.png", Nothing)

blockPuImage :: ImageSpec
blockPuImage = (gameSpecificPath ++ "blockP.png", Nothing)

paddleImage :: ImageSpec
paddleImage = (gameSpecificPath ++ "paddleBluA.png", Nothing)

pointsUpImage :: ImageSpec
pointsUpImage = (gameSpecificPath ++ "diamond-alpha.png", Nothing)

livesUpImage :: ImageSpec
livesUpImage = (gameSpecificPath ++ "heart-alpha.png", Nothing)

mockUpImage :: ImageSpec
mockUpImage = (gameSpecificPath ++ "spike-y-alpha.png", Nothing)

destroyBallUpImage :: ImageSpec
destroyBallUpImage = (gameSpecificPath ++ "spike-b-alpha.png", Nothing)

-- * Audio

audioPath :: FilePath
audioPath = basePath ++ "audio/"

-- ** Music

musicPath :: FilePath
musicPath = audioPath ++ "music/"

backgroundMusic :: MusicSpec
backgroundMusic = musicPath ++ "level0.mp3"

-- ** Sounds

soundsPath :: FilePath
soundsPath = audioPath ++ "sounds/"

blockHitSFX :: SoundSpec
blockHitSFX = (soundsPath ++ "196106_aiwha_ding-cc-by.wav", 2000)

-- * Fonts

fontsPath :: FilePath
fontsPath = basePath ++ "fonts/"

fontPath :: FilePath
fontPath = fontsPath ++ "lacuna.ttf"

gameFontSpec :: FontSpec
gameFontSpec = (fontPath, 32)

-- * Colors
fontColor :: ColorSpec
fontColor = (228, 228, 228, 255)

black :: ColorSpec
black = (0, 0, 0, 255)
