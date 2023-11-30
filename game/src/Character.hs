module Character (
  Character :: Int -> Int -> Bool -> Int -> Int -> Int -> Int -> Character
  xPos :: Character -> Int
  yPos :: Character -> Int
  jumping :: Character -> Bool
  jumpHeight :: Character -> Int
  yVelocity :: Character -> Int
  xVelocity :: Character -> Int
  gravity :: Character -> Int
) where

import qualified SDL
import Foreign.C.Types (CInt)

data Character = Character { 
    xPos :: Int
  , yPos :: Int
  , jumping :: Bool
  , jumpHeight :: Int
  , yVelocity :: Int
  , xVelocity :: Int
  , gravity :: Int
  , rectangle :: SDL.Rectangle CInt
}


instance Show Character where
  show (Character x y j jh yv xv g) = "Character { xPos = " ++ show x ++ ", yPos = " ++ show y ++ ", jumping = " ++ show j ++ ", jumpHeight = " ++ show jh ++ ", yVelocity = " ++ show yv ++ ", xVelocity = " ++ show xv ++ ", gravity = " ++ show g ++ " }"


initCharacter :: Character
initCharacter = Character 0 0 False 0 0 0 0 (SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 0 0))

