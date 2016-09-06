{-# LANGUAGE OverloadedStrings #-}
module Types (
    -- * Main monad in which the game runs.
    Game, runGame, GameState(..),
    Options(..), defaultOptions, opt,
    -- * Game types
    MoveState(..), Speed, Player(..), DiskColor(..), Wall(..),
    Grid(..), Collidable, Box(..),
    -- * Maker helpers
    defaultPlayer, defaultGrid,
    -- * Game types interaction functions
    collides
    ) where

import           Foreign.C.Types
import           Linear
import qualified SDL
import Data.Angle
import Control.Monad.State.Lazy (StateT, evalStateT, gets)
import           Linear (V2)
import           Paths_diskworld (getDataFileName)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- * Game State Types

data GameState = GameState
    { options      :: Options
    , gameWindow   :: SDL.Window
    , gameRenderer :: SDL.Renderer
    , gamePlayer   :: Player    
    , gameGrid     :: Grid
    } 

initGameState :: Options -> GameState
initGameState opts = GameState
    { options      = opts
    , gameWindow   = error "No Game Window set."
    , gameRenderer = error "No Game renderer present."
    , gamePlayer   = error "No Game Player Disk defined."
    , gameGrid     = defaultGrid
    }
    

-- | Main monad in which the game runs, holding state of the game over the
-- IO Monad.
type Game = StateT GameState IO

-- | Run the Game.
runGame :: Options -> Game t -> IO t
runGame opts game = do
    -- init window and renderer
    let w = optGameWidth opts
    let h = optGameHeight opts
    let winConf = SDL.defaultWindow
                { SDL.windowInitialSize = V2 w h }
    window <- SDL.createWindow "Disk World" winConf
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    -- init player
    player <- makePlayer renderer (optInitDisk opts)

    let gameState = (initGameState defaultOptions)
                    { gameWindow = window
                    , gameRenderer = renderer
                    , gamePlayer = player
                    }
                    
    -- finally run the game                
    evalStateT game gameState


-- | Make a default player with some setup
makePlayer :: SDL.Renderer -> DiskColor -> IO Player  -- ^ created Player
makePlayer renderer c = do
    file <- getDataFileName (playerSpriteFile c)
    bmp <- SDL.loadBMP file    
    tex <- SDL.createTextureFromSurface renderer bmp
    SDL.freeSurface bmp
    return defaultPlayer
        { pPosition = V2 100 100
        , pTexture = tex
        , pDiskColor = c
        }




-- * Command Line Option types

-- | Type holding game options configurable through command-line arguments or a
-- config file on setup.
-- XXX Incomplete
data Options = Options
    { optGameWidth  :: CInt
    , optGameHeight :: CInt
    , optInitDisk   :: DiskColor
    } deriving Show

-- XXX Dummy, has to be implemented
defaultOptions :: Options
defaultOptions = Options
    { optGameWidth  = 640
    , optGameHeight = 480
    , optInitDisk   = BlueDisk
    }

opt :: (Options -> t) -> Game t
opt f = f <$> gets options

-- * In game types

-- * Player Type


data Player = Player
     { pPosition  :: V2 Float
     , pSpeed     :: Speed
     , pRotation  :: Degrees Float
     , pTexture   :: SDL.Texture
     , pDiskColor :: DiskColor
     , pState     :: MoveState
     }


instance Show Player where
    show p = "P<" ++ show (pDiskColor p) ++ ">"


defaultPlayer :: Player
defaultPlayer = Player {
  pPosition = error $ "Position has to be specified",
  pSpeed = (1.0, 1.0),
  pRotation = Degrees 0,
  pTexture = error $ "No texture specified.",
  pDiskColor = error $ "No color specified.",
  pState = Rotate
}


data DiskColor = RedDisk | BlueDisk
          deriving (Eq, Ord)


instance Show DiskColor where
    show RedDisk = "Red"
    show BlueDisk = "Blue"


-- | Decide the player sprite given it's color
playerSpriteFile :: DiskColor -> FilePath
playerSpriteFile c = fromMaybe (error ("Disk Color Unkown" ++ show c))
    (Map.lookup c colorMap)


colorMap :: Map.Map DiskColor FilePath
colorMap =
  Map.fromList [ (RedDisk, "assets/disk_blue.bmp")
               , (BlueDisk, "assets/disk_blue.bmp")
               ]


-- * Game movements

data MoveState = Rotate | Move


type Speed = (Float, Float)



-- * Game elements other than the players


data Wall =  TopWall
          | RightWall
          | BottomWall
          | LeftWall
          deriving (Show, Ord, Eq)


data Grid = Grid [Wall]
            deriving (Show, Ord, Eq)


-- | Define the default map boundaries for the game
defaultGrid :: Grid
defaultGrid = Grid [TopWall, RightWall, BottomWall, LeftWall]



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
(width, height) = (fromIntegral 640, fromIntegral 480)



-- | Wall is collidable
instance Collidable Wall where
    getBox TopWall = Box 0 0 width 0
    getBox RightWall = Box width 0 0 height
    getBox BottomWall = Box 0 height width 0
    getBox LeftWall = Box 0 0 0 height

-- | Player is collidable
instance Collidable Player where
    getBox player = let V2 x y = pPosition player in Box x y 32 32
