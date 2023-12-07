module GameRectangle(
  getRectangleX,
  getRectangleY,
  getRectangleWidth,
  getRectangleHeight,
  updateRectangleX,
  updateRectangleY,
  updateRectangleWidth,
  updateRectangleHeight,
  createRectangle,
  hasCollided,
  hasCollidedWithAny,
  hasCollidedWithApple,
  hasCollidedWith
) where

import Foreign.C.Types (CInt)
import qualified SDL

-- Extract x coordinate from SDL.Rectangle
getRectangleX :: Integral a => SDL.Rectangle a -> Int
getRectangleX (SDL.Rectangle (SDL.P (SDL.V2 x _)) _) = fromIntegral x

-- Extract y coordinate from SDL.Rectangle
getRectangleY :: Integral a => SDL.Rectangle a -> Int
getRectangleY (SDL.Rectangle (SDL.P (SDL.V2 _ y)) _) = fromIntegral y

-- Extract width from SDL.Rectangle
getRectangleWidth :: Integral a => SDL.Rectangle a -> Int
getRectangleWidth (SDL.Rectangle _ (SDL.V2 w _)) = fromIntegral w

-- Extract height from SDL.Rectangle
getRectangleHeight :: Integral a => SDL.Rectangle a -> Int
getRectangleHeight (SDL.Rectangle _ (SDL.V2 _ h)) = fromIntegral h

-- Update x coordinate of SDL.Rectangle
updateRectangleX :: SDL.Rectangle CInt -> Int -> SDL.Rectangle CInt
updateRectangleX r x = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral (getRectangleY r)))) (SDL.V2 (fromIntegral (getRectangleWidth r)) (fromIntegral (getRectangleHeight r)))

-- Update y coordinate of SDL.Rectangle
updateRectangleY :: SDL.Rectangle CInt -> Int -> SDL.Rectangle CInt
updateRectangleY r y = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral (getRectangleX r)) (fromIntegral y))) (SDL.V2 (fromIntegral (getRectangleWidth r)) (fromIntegral (getRectangleHeight r)))

-- Update width of SDL.Rectangle
updateRectangleWidth :: SDL.Rectangle CInt -> Int -> SDL.Rectangle CInt
updateRectangleWidth r w = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral (getRectangleX r)) (fromIntegral (getRectangleY r)))) (SDL.V2 (fromIntegral w) (fromIntegral (getRectangleHeight r)))

-- Update height of SDL.Rectangle
updateRectangleHeight :: SDL.Rectangle CInt -> Int -> SDL.Rectangle CInt
updateRectangleHeight r h = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral (getRectangleX r)) (fromIntegral (getRectangleY r)))) (SDL.V2 (fromIntegral (getRectangleWidth r)) (fromIntegral h))

-- Create SDL.Rectangle
createRectangle :: Int -> Int -> Int -> Int -> SDL.Rectangle CInt
createRectangle x y w h = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 (fromIntegral w) (fromIntegral h))

-- Detect collision between two SDL.Rectangles
hasCollided :: SDL.Rectangle CInt -> SDL.Rectangle CInt -> Bool
hasCollided r1 r2 = (getRectangleX r1 < getRectangleX r2 + getRectangleWidth r2) && (getRectangleX r1 + getRectangleWidth r1 > getRectangleX r2) && (getRectangleY r1 < getRectangleY r2 + getRectangleHeight r2) && (getRectangleY r1 + getRectangleHeight r1 > getRectangleY r2)

-- Detect collision between one SDL.Rectangle and a list of SDL.Rectangles
hasCollidedWithAny :: SDL.Rectangle CInt -> [SDL.Rectangle CInt] -> Bool
hasCollidedWithAny _ [] = False
hasCollidedWithAny r (x:xs) = hasCollided r x || hasCollidedWithAny r xs


hasCollidedWithApple :: SDL.Rectangle CInt -> [SDL.Rectangle CInt] -> (Bool, SDL.Rectangle CInt)
hasCollidedWithApple _ [] = (False, SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 0 0))
hasCollidedWithApple r (x:xs) =
  if hasCollided r x
    then (True, x)
    else hasCollidedWithApple r xs





-- Detect collision between two SDL.Rectangles and return the SDL.Rectangle that was collided with
hasCollidedWith :: SDL.Rectangle CInt -> [SDL.Rectangle CInt] -> Maybe (SDL.Rectangle CInt)
hasCollidedWith _ [] = Nothing
hasCollidedWith r (x:xs) = if hasCollided r x then Just x else hasCollidedWith r xs
