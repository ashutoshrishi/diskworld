module World where

import           Prelude         hiding (init)
import           SDL             (($=))
import qualified SDL

import qualified Data.Map        as Map
import           Foreign.C.Types
import           Graphics
import           Types
import Linear
import Data.Angle        


playerSpriteFile :: Color -> FilePath
playerSpriteFile color = case (Map.lookup color colorMap) of
                           Just f -> f
                           Nothing -> error "Color not found."


movePlayer :: Player -> Player
movePlayer p = let i = position p
                   r = rotation p
                   s = speed p
               in p { position = updatePosition i r s }


updatePosition :: V2 Float -> Degrees Float -> Speed -> V2 Float
updatePosition (V2 x y) d (sx, sy) =
    let x' = x + (sx * (sine d))
        y' = y - (sy * (cosine d))
    in V2 x' y'
                  

rotatePlayer :: Player -> Player
rotatePlayer p = let Degrees oldAngle = rotation p
                 in p { rotation = Degrees (oldAngle + 0.5) }
