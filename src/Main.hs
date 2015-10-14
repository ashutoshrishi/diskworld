{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where


import           Control.Applicative
import           Control.Monad       (unless)
import           Data.Monoid
import           Foreign.C.Types
import           Linear
import           Linear.Affine       (Point (P))

import           Prelude             hiding (init)
import           SDL                 (($=))
import qualified SDL

import           Game
import           Graphics
import           Paths_diskworld     (getDataFileName)
import           Types


main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]
  -- init window and renderer
  window <- SDL.createWindow "Lesson 4a" gameWindowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  world <- startWorld renderer (V2 100 100) Blue
  gameLoop renderer world

  -- destroy
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
