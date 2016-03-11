module Types where

import           Foreign.C.Types
import           Linear
import           Prelude         hiding (init)
import qualified SDL
import Data.Angle

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)


data MoveState = Rotate | Move

data World = World
             { player :: Player
             , currentState :: MoveState
             , grid :: Grid }

type Speed = (Float, Float)

data Player = Player
     { position  :: V2 Float
     , speed     :: Speed
     , rotation  :: Degrees Float
     , texture   :: SDL.Texture
     , diskColor :: Color
     }


defaultPlayer :: Player
defaultPlayer = Player {
  position = error $ "Position has to be specified",
  speed = (1.0, 1.0),
  rotation = Degrees 0,
  texture = error $ "No texture specified.",
  diskColor = error $ "No color specified"
}

data Color = Red | Blue
           deriving (Show, Ord, Eq)


data Wall =  TopWall 
          | RightWall
          | BottomWall 
          | LeftWall
          deriving (Show, Ord, Eq)

data Grid = Grid [Wall]
            deriving (Show, Ord, Eq)


-- | Type class encompassing physical collidable objects, whose
-- collision boundaries are represented as a quadrilateral.
class Collidable a where
    getBox :: a -> Box

collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides x y = boxCollision (getBox x) (getBox y)


-- | Bounding collision box 
data Box = NoBox | Box
                   { topEdge    :: Float
                   , rightEdge  :: Float
                   , bottomEdge :: Float
                   , leftEdge   :: Float
                   } deriving (Show, Ord, Eq)

-- | Test for collision between two Boxes.
boxCollision :: Box -> Box -> Bool
boxCollision (Box x1 y1 w1 h1) (Box x2 y2 w2 h2) =
    x1 < (x2 + w2)
           && (x1 + w1) > x2
           && (y1 < (y2 + h2))
           && ((y1 + h1) > y2)
                   

-- | Floating point conversion of the height and width of the game
-- screen.
width, height :: Float                   
(width, height) = (fromIntegral screenWidth, fromIntegral screenHeight)



-- | Wall is collidable
instance Collidable Wall where
    getBox TopWall = Box 0 0 width 0
    getBox RightWall = Box width 0 0 height
    getBox BottomWall = Box 0 height width 0
    getBox LeftWall = Box 0 0 0 height

-- | Player is collidable
instance Collidable Player where
    getBox player = let V2 x y = position player in Box x y 32 32
    





