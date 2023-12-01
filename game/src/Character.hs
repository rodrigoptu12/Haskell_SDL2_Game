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

import Foreign.C.Types (CInt)
import GameRectangle as GR
import Events

import qualified Map as M
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
createCharacter x y j jh yv xv g = Character x y j jh yv xv g (GR.createRectangle x y 32 32)

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

gravityLogic :: Character -> [M.Block] -> Character
gravityLogic c blocks 
  | not collided = updateCharacterYPos c (yPos c + gravity c)
  | otherwise = c
  where bRects = M.getCollisionBlockRectangles blocks
        cRect = rectangle c
        collided = GR.hasCollidedWithAny cRect bRects

moveLogic :: Character -> [M.Block] -> GameEvent -> Character
-- considere direita ou esquerda
moveLogic c blocks g
  | not collided = if g == LeftPressed then updateCharacterXPos c (xPos c - xVelocity c) else updateCharacterXPos c (xPos c + xVelocity c)
  | otherwise = c
  where 
        groundBlocks = M.getCollisionGroundBlocks blocks
        bRects = M.getCollisionBlockRectangles groundBlocks
        cRect = rectangle c
        collided = GR.hasCollidedWithAny cRect bRects
  
        
        --  se jumping == true e jumpHeight > 0 entao yPos = yPos - yVelocity e jumpHeight = jumpHeight - 1
        --  se jumping == true e jumpHeight == 0 entao jumping = false
jumpLogic :: Character -> [M.Block] -> Character
jumpLogic c blocks 
  | jumping c && jumpHeight c > 0 =  updateCharacterYPos c {jumping = True, jumpHeight = jumpHeight c - 1} (yPos c - yVelocity c) 
  | otherwise = c {jumping = False, jumpHeight = 10}
  where bRects = M.getCollisionBlockRectangles blocks
        cRect = rectangle c
        collided = GR.hasCollidedWithAny cRect bRects

updateCharacterWithEvent :: Character -> GameEvent -> [M.Block] -> Character
updateCharacterWithEvent c g blocks = case g of
  UpPressed -> jumpLogic shouldJump  blocks  
  LeftPressed -> moveLogic c' blocks g
  RightPressed -> moveLogic c' blocks g
  DownPressed -> c'
  _ ->  c'
  where 
    c' = gravityLogic c blocks
    groundBlocks = M.getCollisionGroundBlocks blocks
    bRects = M.getCollisionBlockRectangles groundBlocks
    cRect = rectangle c
    isGround = GR.hasCollidedWithAny cRect bRects
    shouldJump = if UpPressed == g then if jumping c' && isGround then c' else c' {jumping = True, jumpHeight = 10} else c'

     

updateCharacterWithEvents :: Character -> [GameEvent] -> [M.Block] -> Character
updateCharacterWithEvents c [] blocks = updateCharacterWithEvent c NotImplemented blocks
updateCharacterWithEvents c (x:xs) blocks = updateCharacterWithEvents (updateCharacterWithEvent c x blocks) xs blocks