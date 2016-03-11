{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graphics where

import           Control.Applicative
import           Control.Monad (unless)
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


-- | Derive the default player's starting position according to screen
-- size.
playerStartX, playerStartY :: CInt
(playerStartX, playerStartY) = let x = (screenWidth `div` 2)
                                   y = (screenHeight `div` 2)
                               in (x,y)



data RenderPos = At (Point V2 CInt) CDouble


gameWindowConfig :: SDL.WindowConfig
gameWindowConfig = SDL.defaultWindow  {
  SDL.windowInitialSize = V2 screenWidth screenHeight }


colorMap :: Map.Map Color FilePath
colorMap =
  Map.fromList [ (Red, "assets/disk_blue.bmp")
               , (Blue, "assets/disk_blue.bmp")
               ]


loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path = do
  bmp <- SDL.loadBMP path
  SDL.createTextureFromSurface renderer bmp <* SDL.freeSurface bmp


renderTexture :: SDL.Renderer -> SDL.Texture -> RenderPos -> IO ()
renderTexture renderer tex (At p r) = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      extent = (V2 w h)
      flip = V2 False False      
  SDL.copyEx renderer tex Nothing (Just $ SDL.Rectangle p extent) r Nothing flip




----------------------------------------------------------------------------
-- Drawing                                                                --
----------------------------------------------------------------------------

drawWorld :: SDL.Renderer -> World -> IO ()
drawWorld renderer world = do
  let disk = (texture . player) world
  let V2 x y = (position . player) world
  let pos = V2 (CInt $ round x) (CInt $ round y)
  let Degrees rotAngle = (rotation . player) world
  let rot = (CDouble . float2Double) rotAngle
  -- drawing
  SDL.clear renderer
  -- SDL.rendererDrawColor renderer $= V4 255 255 255 255
  renderTexture renderer disk $ At (P pos) rot
  SDL.present renderer
