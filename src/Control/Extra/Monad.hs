-- |
-- Copyright  : (c) Ivan Perez & Henrik Nilsson, 2014.
-- License    : See LICENSE file.
-- Maintainer : Ivan Perez <ivan.perez@keera.co.uk>
--
-- Auxiliary functions related to Control.Monad.
module Control.Extra.Monad where

-- External imports
import Control.Monad

whileLoopM :: Monad m => m a -> (a -> Bool) -> (a -> m ()) -> m ()
whileLoopM val cond act = r'
  where r' = do v <- val
                when (cond v) $ do
                  act v
                  whileLoopM val cond act

foldLoopM :: Monad m => a -> m b -> (b -> Bool) -> (a -> b -> m a) -> m a
foldLoopM val sense cond act = r'
  where r' = do s <- sense
                if cond s
                  then do
                    val' <- act val s
                    foldLoopM val' sense cond act
                  else return val
