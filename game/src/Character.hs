module Character (
  Character(..),  -- Expose the Character data type and its constructors
  initCharacter,  -- Expose the initCharacter function
  xPos,
  yPos,
  jumping,
  jumpHeight,
  yVelocity,
  xVelocity,
  gravity,
  rectangle
) where

import qualified SDL
import Foreign.C.Types (CInt)

data Character = Character {
  xPos :: Int,
  yPos :: Int,
  jumping :: Bool,
  jumpHeight :: Int,
  yVelocity :: Int,
  xVelocity :: Int,
  gravity :: Int,
  rectangle :: SDL.Rectangle CInt
} deriving (Eq)

instance Show Character where
  show (Character x y j jh yv xv g r) = "Character { xPos = " ++ show x ++ ", yPos = " ++ show y ++ ", jumping = " ++ show j ++ ", jumpHeight = " ++ show jh ++ ", yVelocity = " ++ show yv ++ ", xVelocity = " ++ show xv ++ ", gravity = " ++ show g ++ ", rectangle = " ++ show r ++ " }"

initCharacter :: Int -> Int -> Bool -> Int -> Int -> Int -> Int -> Character
initCharacter x y j jh yv xv g = Character x y j jh yv xv g (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 32 32))
