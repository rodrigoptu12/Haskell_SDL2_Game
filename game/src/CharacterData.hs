module CharacterData where


import Foreign.C.Types (CInt)
import qualified SDL

data Character = Character
  { xPos :: Int,
    yPos :: Int,
    jumping :: Bool,
    jumpHeight :: Int,
    yVelocity :: Int,
    xVelocity :: Int,
    gravity :: Int,
    rectangle :: SDL.Rectangle CInt,
    leftPressed :: Bool,
    rightPressed :: Bool,
    upPressed :: Bool,
    downPressed :: Bool
  }
  deriving (Eq)
  