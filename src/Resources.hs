-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Resource specifications.
module Resources where

data ResourceSpec = ResourceSpec
  { fonts  :: [FontResource]
  , images :: [ImageResource]
  , music  :: [MusicResource]
  , audio  :: [AudioResource]
  }

type FontResource = Resource

type ImageResource = Resource

type MusicResource = Resource

type AudioResource = Resource

newtype Resource = Resource { _resourceFP :: FilePath }
  deriving Eq
