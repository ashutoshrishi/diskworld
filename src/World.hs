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


defaultGrid :: Grid
defaultGrid = Grid [TopWall, RightWall, BottomWall, LeftWall]
    
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
                 in p { rotation = Degrees (incAngle oldAngle) }

incAngle :: Float -> Float
incAngle a = fromIntegral $ ((ceiling a') `mod` 360)
    where a' = a + 0.5

flipAngle :: World -> World
flipAngle world =
    let p = player world
        Degrees a = rotation p
        a' = 180 - a
        a'' = if a' < 0 then (360 + a') else a'
        r' = Degrees a''
        p' = p { rotation = r' }
    in world { player = p' }

                    

----------------------------------------------------------------------------
-- Collision Manager                                                      --
----------------------------------------------------------------------------

playerBox :: Player -> Box
playerBox player = Box x y 32 32
    where V2 x y = position player


width, height :: Float                   
(width, height) = (fromIntegral screenWidth, fromIntegral screenHeight)
                   
wallBox :: Wall -> Box
wallBox TopWall = Box 0 0 width 0
wallBox RightWall = Box width 0 0 height
wallBox BottomWall = Box 0 height width 0
wallBox LeftWall = Box 0 0 0 height
                   

boxCollision :: Box -> Box -> Bool
boxCollision (Box x1 y1 w1 h1) (Box x2 y2 w2 h2) =
    x1 < (x2 + w2)
           && (x1 + w1) > x2
           && (y1 < (y2 + h2))
           && ((y1 + h1) > y2)
    
runCollisionManager :: World -> IO World
runCollisionManager w =
    do let p = player w
       let Grid walls = grid w
       case any (playerWallCollision p) walls of
         True -> return (flipAngle w)
         False -> return w                  

playerWallCollision :: Player -> Wall -> Bool
playerWallCollision p w = boxCollision (playerBox p) (wallBox w)

                          
                       
