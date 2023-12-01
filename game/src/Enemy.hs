module Enemy(
  Enemy (..), -- Expose the Enemy data type and its constructors
  createEnemy, -- Expose the createEnemy function
  updateEnemyXPos,
  updateEnemyYPos,
  updateEnemyJumping,
  updateEnemyJumpHeight,
  updateEnemyYVelocity,
  updateEnemyXVelocity,
  updateEnemyGravity
) where

import Foreign.C.Types (CInt)
import GameRectangle as GR
import qualified SDL

data Enemy = Enemy
  { xPos :: Int,
    yPos :: Int,
    jumping :: Bool,
    jumpHeight :: Int,
    yVelocity :: Int,
    xVelocity :: Int,
    gravity :: Int,
    rectangle :: SDL.Rectangle CInt
  }
  deriving (Eq)

instance Show Enemy where
  show (Enemy x y j jh yv xv g r) = "Enemy { xPos = " ++ show x ++ ", yPos = " ++ show y ++ ", jumping = " ++ show j ++ ", jumpHeight = " ++ show jh ++ ", yVelocity = " ++ show yv ++ ", xVelocity = " ++ show xv ++ ", gravity = " ++ show g ++ ", rectangle = " ++ show r ++ " }"

createEnemy :: Int -> Int -> Bool -> Int -> Int -> Int -> Int -> Enemy
createEnemy x y j jh yv xv g = Enemy x y j jh yv xv g (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 32 32))

updateEnemyXPos :: Enemy -> Int -> Enemy
updateEnemyXPos c x = c {xPos = x, rectangle = GR.updateRectangleX (rectangle c) x}

updateEnemyYPos :: Enemy -> Int -> Enemy
updateEnemyYPos c y = c {yPos = y, rectangle = GR.updateRectangleY (rectangle c) y}

updateEnemyJumping :: Enemy -> Bool -> Enemy
updateEnemyJumping c j = c {jumping = j}

updateEnemyJumpHeight :: Enemy -> Int -> Enemy
updateEnemyJumpHeight c jh = c {jumpHeight = jh}

updateEnemyYVelocity :: Enemy -> Int -> Enemy
updateEnemyYVelocity c yv = c {yVelocity = yv}

updateEnemyXVelocity :: Enemy -> Int -> Enemy
updateEnemyXVelocity c xv = c {xVelocity = xv}

updateEnemyGravity :: Enemy -> Int -> Enemy
updateEnemyGravity c g = c {gravity = g}
