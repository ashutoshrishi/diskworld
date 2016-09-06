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
import Control.Monad.State.Lazy (gets)
import           Types


main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]
  runGame defaultOptions mainGame


mainGame :: Game ()
mainGame = do
    setupWorld
    gameLoop
    -- cleanup
    renderer <- gets gameRenderer
    window <- gets gameWindow
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
