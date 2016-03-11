module World (
    withPlayer, movePlayer, rotatePlayer, makeVelocityVector,
    runCollisionManager
             ) where

import           Prelude         hiding (init)

import           Data.Angle
import           Data.Map        as Map
import           Foreign.C.Types
import           Graphics
import           Linear
import           Types


-- | Update the world's current player and place it back in the
-- world.
withPlayer :: World -> (Player -> Player) -> World
withPlayer w f = w { player = player' }
    where player' = f (player w)


-- | Move a player forward in the direction the disk is facing.
-- No rotation is to take place at this step.
movePlayer :: Player -> Player
movePlayer p =
  let oldp = position p
      vel = makeVelocityVector (speed p) (rotation p)
  in p { position = oldp + vel }


-- | Rotate the disk at its place gradually.
rotatePlayer :: Player -> Player
rotatePlayer p = let Degrees oldAngle = rotation p
                 in p { rotation = Degrees (incAngle oldAngle) }

-- | Increment angle by a fixed graduating amount.
incAngle :: Float -> Float
incAngle a = fromIntegral $ ((ceiling a') `mod` 360)
    where a' = a + 0.5

-- | Compute the veclocity vector for the player, which
-- decide the player's movement in each direction every update.
makeVelocityVector :: Speed
                   -> Degrees Float
                   -> V2 Float
makeVelocityVector (sx, sy) d =
  V2 (sx * cosine d) (sy * sine d)

-- | Compute the re-bound angle for the player disk when
-- it collides at a point with the given normal.
-- The incoming disk angle is complimented, it's angle with with
-- the normal is calculated and that amount is added/subtracted
-- from the normal to get the outgoing angle.
flipAngle :: Degrees Float -- ^ rotation of disk
          -> Degrees Float -- ^ normal
          -> Degrees Float -- ^ rebound rotation
flipAngle inc@(Degrees a) norm@(Degrees n) =
  if comp > n
  then bounded $ norm - out
  else bounded $ norm + out
  where
    Degrees comp = compliment inc
    out = vectorAngle (angle' comp) (angle' n)

-- | Vector dot product two vectors.
dotProd :: V2 Float ->  V2 Float -> Float
dotProd a b = sum $ a * b

-- | Magnitude of a vector
magnitude :: V2 Float -> Float
magnitude (V2 a b) = sqrt $ (a * a) + (b * b)

-- | Create a unit polar vector from an angle in degrees.
angle' :: Floating a => a -> V2 a
angle' a = angle $ a * (pi / 180)

-- | Calculate angle between two vectors using dot product.
vectorAngle :: V2 Float -> V2 Float -> Degrees Float
vectorAngle v n = degrees r
  where
    r = Radians $ acos $ (dotProd n v) / (magnitude v)

-- | Compliment of an angle.
compliment :: Degrees Float -> Degrees Float
compliment (Degrees a) = bounded $ Degrees (a + 180)

-- | Constrain the angle within the choosen convention of
-- clockwise rotaion from 0 degrees to 359 degrees.
bounded :: Degrees Float -> Degrees Float
bounded d@(Degrees a)
  | a < 0 = Degrees $ a + 360
  | otherwise = if a >= 360 then Degrees $ a - 360 else d

----------------------------------------------------------------------------
-- Collision Manager                                                      --
----------------------------------------------------------------------------

-- | The collision manager run, checks if the current position of the
-- player disc collides with any fixed collidable boxes withing the
-- game, and simulates 2d physics on collision.
runCollisionManager :: World -> IO World
runCollisionManager w =
    do let p = player w
       let Grid walls = grid w
       let world' = withPlayer w (checkWalls walls)
       return world'

-- | Checks player collision with a list of walls, and on collision
-- bounces the ball according to some 2d physics.
checkWalls :: [Wall] -> Player -> Player
checkWalls [] p = p
checkWalls (w:ws) p =
  if collides w p
  then checkWalls ws (bounce w p)
  else checkWalls ws p


-- | Simulate a player bouncing off the given wall, while
-- considering the approach angle
bounce :: Wall -> Player -> Player
bounce wall p = p { rotation = newAngle }
  where
    oldAngle = rotation p
    newAngle = flipAngle oldAngle (normal wall)


-- | Get the normal vector angle of the given Wall
-- (with respect to the disk frame of reference)
normal :: Wall -> Degrees Float
normal TopWall = Degrees 90
normal RightWall = Degrees 180
normal BottomWall = Degrees 270
normal LeftWall = Degrees 360
