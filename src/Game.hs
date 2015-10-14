module Game where

import           Control.Monad (unless)
import           Foreign.C.Types
import           Linear
import           Paths_diskworld (getDataFileName)
import           Prelude hiding (init)
import           SDL (($=))
import qualified SDL

import           Data.Angle
import           Graphics
import           Types
import           World

-- | Make a default player with some setup
makePlayer :: V2 Float    -- ^ Player position
           -> Speed
           -> SDL.Texture   -- ^ Player color for sprite selection
           -> Color
           -> Player  -- ^ created Player
makePlayer pos sp tex c =
  defaultPlayer
  { position = pos
  , speed = sp
  , texture = tex
  , diskColor = c
  }


defaultSpeed :: (Float, Float)
defaultSpeed = let sc = 0.01
                   -- sx = sc * (fromIntegral screenWidth)
                   -- sy = sc * (fromIntegral screenHeight)
                   sx = 0.5
                   sy = 0.5
               in (sx, sy)

startWorld :: SDL.Renderer -> V2 Float -> Color -> IO World
startWorld renderer pos color = do
  sprite <- getDataFileName (playerSpriteFile color) >>= loadTexture renderer
  let player = makePlayer pos defaultSpeed sprite color
  let world = World player Rotate defaultGrid
  return world


----------------------------------------------------------------------------
-- Logic                                                                  --
----------------------------------------------------------------------------

gameLoop :: SDL.Renderer -> World -> IO ()
gameLoop renderer world = do
  let Degrees angle = rotation (player world)
  putStrLn $ "Angle: " ++ (show angle)
  world <- runCollisionManager world
  drawWorld renderer world
  events <- SDL.pollEvents

  let qPressed = keycodeOccurs SDL.KeycodeQ events
  -- Let the events decide the resulting state of the world
  let world' = eventHandler world events   
  unless qPressed (gameLoop renderer (runWorld world'))


moveState :: World -> World            
moveState world = world { currentState = Move }

rotateState :: World -> World
rotateState world = world { currentState = Rotate }

runWorld :: World -> World
runWorld world = case currentState world of
                       Rotate -> withPlayer world rotatePlayer
                       Move -> withPlayer world movePlayer

withPlayer :: World -> (Player -> Player) -> World
withPlayer w f = w { player = player' }
    where player' = f (player w)

----------------------------------------------------------------------------
-- Event handling                                                         --
----------------------------------------------------------------------------

eventHandler :: World -> [SDL.Event] -> World
eventHandler world [] = world
eventHandler world (e:es)
    | pressEventOf SDL.KeycodeA e = moveState world
    | releaseEventOf SDL.KeycodeA e = rotateState world
    | otherwise = eventHandler world es
               

keycodeOccurs :: SDL.Keycode -> [SDL.Event] -> Bool
keycodeOccurs code events = not (null $ filter (pressEventOf code) events)

pressEventOf :: SDL.Keycode -> SDL.Event -> Bool
pressEventOf code event =
  case (SDL.eventPayload event) of
    SDL.KeyboardEvent ke -> (isPressed ke) && (isCode ke code)
    _ -> False
  where
    isPressed ke = SDL.keyboardEventKeyMotion ke == SDL.Pressed
    isCode ke c = SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == c


releaseEventOf :: SDL.Keycode -> SDL.Event -> Bool
releaseEventOf code event =
  case (SDL.eventPayload event) of
    SDL.KeyboardEvent ke -> (isReleased ke) && (isCode ke code)
    _ -> False
  where
    isReleased ke = SDL.keyboardEventKeyMotion ke == SDL.Released
    isCode ke c = SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == c
                  

                  
heldEventOf :: SDL.Keycode -> SDL.Event -> Bool
heldEventOf code event =
  case (SDL.eventPayload event) of
    SDL.KeyboardEvent ke -> (isCode ke code) && ((isHeld ke) || (isPressed ke))
    _ -> False
  where
    isPressed ke = SDL.keyboardEventKeyMotion ke == SDL.Pressed
    isHeld ke = SDL.keyboardEventRepeat ke 
    isCode ke c = SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == c

