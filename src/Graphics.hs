{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graphics where

import           Control.Applicative
import           Control.Monad (unless)
import Control.Monad.State.Lazy (gets)
import           Data.Monoid
import           Foreign.C.Types
import           Linear
import           Linear (V4 (..))
import           Linear.Affine (Point (P))

import           SDL (($=))
import qualified SDL

import           Data.Angle
import qualified Data.Map as Map
import           GHC.Float
import           Paths_diskworld (getDataFileName)
import           Types
import Data.Maybe (fromMaybe)


-- | Derive the default player's starting position according to screen
-- size.
playerStartX, playerStartY :: CInt
(playerStartX, playerStartY) = let x = (640 `div` 2)
                                   y = (480 `div` 2)
                               in (x,y)



data RenderPos = At (Point V2 CInt) CDouble


loadTexture :: FilePath -> Game SDL.Texture
loadTexture path = do
    renderer <- gets gameRenderer
    bmp <- SDL.loadBMP path
    SDL.createTextureFromSurface renderer bmp <* SDL.freeSurface bmp


renderTexture :: SDL.Texture -> RenderPos -> Game ()
renderTexture tex (At p r) = do
    renderer <- gets gameRenderer
    ti <- SDL.queryTexture tex
    let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
    let extent = (V2 w h)
    let flipF = V2 False False
    let rect = Just $ SDL.Rectangle p extent
    SDL.copyEx renderer tex Nothing
        rect r Nothing flipF




----------------------------------------------------------------------------
-- Drawing                                                                --
----------------------------------------------------------------------------

-- drawWorld :: SDL.Renderer -> World -> IO ()
-- drawWorld renderer world = do
--   let disk = (pTexture . player) world
--   let V2 x y = (pPosition . player) world
--   let pos = V2 (CInt $ round x) (CInt $ round y)
--   let Degrees rotAngle = (pRotation . player) world
--   let rot = (CDouble . float2Double) rotAngle
--   -- drawing
--   SDL.clear renderer
--   -- SDL.rendererDrawColor renderer $= V4 255 255 255 255
--   renderTexture renderer disk $ At (P pos) rot
--   SDL.present renderer


drawWorld :: Game ()
drawWorld = do
    renderer <- gets gameRenderer
    player <- gets gamePlayer
    let disk = pTexture player
    let V2 x y = pPosition player
    let pos = V2 (CInt $ round x) (CInt $ round y)
    let Degrees rotAngle = pRotation player
    let rot = (CDouble . float2Double) rotAngle
    -- drawing
    SDL.clear renderer
    -- SDL.rendererDrawColor renderer $= V4 255 255 255 255
    renderTexture disk $ At (P pos) rot
    SDL.present renderer
