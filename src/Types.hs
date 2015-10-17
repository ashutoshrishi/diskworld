module Types where

import           Foreign.C.Types
import           Linear
import           Prelude         hiding (init)
import qualified SDL
import Data.Angle


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


-- | Bounding collision box 
data Box = NoBox | Box
                   { topEdge    :: Float
                   , rightEdge  :: Float
                   , bottomEdge :: Float
                   , leftEdge   :: Float
                   } deriving (Show, Ord, Eq)

data Wall =  TopWall 
          | RightWall
          | BottomWall 
          | LeftWall
          deriving (Show, Ord, Eq)

data Grid = Grid [Wall]
            deriving (Show, Ord, Eq)


