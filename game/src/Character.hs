module Character
  ( Character (..), -- Expose the Character data type and its constructors
    createCharacter, -- Expose the createCharacter function
    updateCharacterXPos,
    updateCharacterYPos,
    updateCharacterJumping,
    updateCharacterJumpHeight,
    updateCharacterYVelocity,
    updateCharacterXVelocity,
    updateCharacterGravity,
  )
where

import Foreign.C.Types (CInt)
import GameRectangle as GR
import qualified SDL

data Character = Character
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

instance Show Character where
  show (Character x y j jh yv xv g r) = "Character { xPos = " ++ show x ++ ", yPos = " ++ show y ++ ", jumping = " ++ show j ++ ", jumpHeight = " ++ show jh ++ ", yVelocity = " ++ show yv ++ ", xVelocity = " ++ show xv ++ ", gravity = " ++ show g ++ ", rectangle = " ++ show r ++ " }"

createCharacter :: Int -> Int -> Bool -> Int -> Int -> Int -> Int -> Character
createCharacter x y j jh yv xv g = Character x y j jh yv xv g (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 32 32))

updateCharacterXPos :: Character -> Int -> Character
updateCharacterXPos c x = c {xPos = x, rectangle = GR.updateRectangleX (rectangle c) x}

updateCharacterYPos :: Character -> Int -> Character
updateCharacterYPos c y = c {yPos = y, rectangle = GR.updateRectangleY (rectangle c) y}

updateCharacterJumping :: Character -> Bool -> Character
updateCharacterJumping c j = c {jumping = j}

updateCharacterJumpHeight :: Character -> Int -> Character
updateCharacterJumpHeight c jh = c {jumpHeight = jh}

updateCharacterYVelocity :: Character -> Int -> Character
updateCharacterYVelocity c yv = c {yVelocity = yv}

updateCharacterXVelocity :: Character -> Int -> Character
updateCharacterXVelocity c xv = c {xVelocity = xv}

updateCharacterGravity :: Character -> Int -> Character
updateCharacterGravity c g = c {gravity = g}
