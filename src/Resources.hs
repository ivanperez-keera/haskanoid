module Resources where

data ResourceSpec = ResourceSpec
 { fonts  :: [FontResource]
 , images :: [ImageResource]
 , music  :: [MusicResource]
 , audio  :: [AudioResource]
 }

type FontResource  = Resource
type ImageResource = Resource
type MusicResource = Resource
type AudioResource = Resource

newtype Resource = Resource { _resourceFP :: FilePath }
 deriving Eq
