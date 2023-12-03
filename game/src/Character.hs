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
    updateCharacterWithEvents, 
    gravityLogic,
  )
where

import GameRectangle as GR
import Events
import qualified Map as M
import qualified SDL
import CharacterData

instance Show Character where
  show (Character x y j jh yv xv g r le ri up dow) = "Character { xPos = " ++ show x ++ ", yPos = " ++ show y ++ ", jumping = " ++ show j ++ ", jumpHeight = " ++ show jh ++ ", yVelocity = " ++ show yv ++ ", xVelocity = " ++ show xv ++ ", gravity = " ++ show g ++ ", rectangle = " ++ show r ++ " } "

createCharacter :: Int -> Int -> Bool -> Int -> Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Character
createCharacter x y j jh yv xv g = Character x y j jh yv xv g (GR.createRectangle x y 32 32)

updateCharacterXPos :: Character  -> Character
updateCharacterXPos c = c {xPos = xPos c + xVelocity c, rectangle = GR.updateRectangleX (rectangle c) (xPos c + xVelocity c)}

updateCharacterYPos :: Character -> Character
updateCharacterYPos c = c {yPos = yPos c - yVelocity c, rectangle = GR.updateRectangleY (rectangle c) (yPos c - yVelocity c)}

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

gravityLogic :: Character -> [M.Block] -> Character
gravityLogic c blocks 
  | not (isGrounded c blocks)  = updateCharacterYPos c' {yVelocity = yVelocity c' - gravity c'}
  | otherwise = c {yVelocity = 0}
  where groundBlocks = M.getCollisionGroundBlocks blocks
        bRects = M.getCollisionBlockRectangles groundBlocks
        cRect = rectangle c
        collided = GR.hasCollidedWithAny cRect bRects
        c' = if collided then c {yVelocity = 0} else c


isGrounded :: Character -> [M.Block] -> Bool
isGrounded c blocks 
  | not collided = False
  | otherwise = True
  where groundBlocks = M.getCollisionGroundBlocks blocks
        bRects = M.getCollisionBlockRectangles groundBlocks
        cRect = rectangle c
        collided = GR.hasCollidedWithAny cRect bRects

moveLogic :: Character -> [M.Block] -> GameEvent -> Character
moveLogic c blocks g = if g == LeftPressed then updateCharacterXPos c {xVelocity = -4} else if g == RightPressed then updateCharacterXPos c {xVelocity = 4} else c

jumpLogic :: [M.Block] -> Character -> Character
jumpLogic blocks c@Character {jumping = True, jumpHeight = jh, yVelocity = vy, xVelocity = vx} =
  if vy > 0
    then let c' = updateCharacterXPos c 
          in updateCharacterYPos c' {jumpHeight = 10, yVelocity = vy - 1} 
    else updateCharacterJumping c False
jumpLogic blocks c@Character {jumping = False} = c 



updateCharacterWithEvents :: Character -> [M.Block] -> Character
updateCharacterWithEvents c blocks 
  | (upPressed c' && isGrounded c' blocks) = jumpLogic blocks shouldJump
  | rightPressed c' = moveLogic c' blocks RightPressed
  | leftPressed c' = moveLogic c' blocks LeftPressed
  | downPressed c' = c'
  | otherwise = c'
  where
    c' = gravityLogic c blocks
    shouldJump = if isGrounded c' blocks && not (jumping c' == True) then (updateCharacterJumping c'{yVelocity = 20} True)  else c'
