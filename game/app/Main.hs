{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image as SDLImage
import Data.IORef
import Foreign.C.Types (CInt)
import Control.Monad (void, unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import SDL.Video.Renderer (Rectangle)
import Control.Monad.Extra    (loopM, whileM)
import Control.Monad.State
import  Prelude                hiding (Left, Right)
import Data.Maybe (mapMaybe)
import GHC.Word (Word32)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)

data Intent
  = Render Direction
  | Quit
  | NotImplemented
  deriving (Show, Eq)


data AssetMap a = AssetMap
  { maskDude  :: a
  , backGround :: a
  } deriving (Foldable, Traversable, Functor)

data Character = Character
 { xPos :: Int
 , yPos :: Int
 , jumping :: Bool
 , jumpHeight :: Int
 , yVelocity :: Int
 , xVelocity :: Int
 , gravity :: Int
 }

surfacePaths :: AssetMap FilePath
surfacePaths = AssetMap
  {
    maskDude  = "./assets/Main Characters/Mask Dude/Jump (32x32).png",
    backGround = "./assets/Background/Blue.png"
  }

-- define a 60 FPS constant
fps :: GHC.Word.Word32
fps = 60
frameDelay :: GHC.Word.Word32
frameDelay = 1000 `div` fps -- 1000 ms / 60 fps = 16.6666 ms

getKey :: SDL.KeyboardEventData -> Maybe Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =  Just $
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeUp     -> Render Up
    SDL.KeycodeDown   -> Render Down
    SDL.KeycodeLeft   -> Render Left
    SDL.KeycodeRight  -> Render Right
    _                 -> NotImplemented

mapEventsToIntents :: [SDL.Event] -> [Intent]
mapEventsToIntents = mapMaybe (payloadToIntent . SDL.eventPayload)
  where
    payloadToIntent :: SDL.EventPayload -> Maybe Intent
    payloadToIntent (SDL.KeyboardEvent k) = getKey k
    payloadToIntent SDL.QuitEvent         = Just Quit
    payloadToIntent _                     = Nothing

updateCharacterPosition :: [Intent] -> Character -> Character
updateCharacterPosition directions character =
    foldl (\acc dir -> case dir of
                Render Left  -> acc { xVelocity = -4, xPos = xPos acc - 4 }  -- Define a velocidade horizontal para a esquerda
                Render Right -> acc { xVelocity = 4, xPos = xPos acc + 4 }   -- Define a velocidade horizontal para a direita
                Render Up    -> if jumping acc then acc else acc { jumping = True, jumpHeight = 10, yVelocity = 13 }  -- Jump
                _     -> acc
          ) character directions

renderCharacter :: (MonadIO m) => SDL.Renderer -> AssetMap SDL.Texture -> Character -> m ()
renderCharacter renderer assets c = liftIO $ do
    let position = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPos c) (fromIntegral $ yPos c))) (SDL.V2 50 50)
    SDL.copy renderer (backGround assets) Nothing Nothing
    SDL.copy renderer (maskDude assets) Nothing (Just position)
    SDL.present renderer

isKeyPressed :: SDL.Scancode -> IO Bool
isKeyPressed scancode = do
  keyboardState <- SDL.getKeyboardState
  return $ keyboardState scancode
updateCharacter :: Character -> [Intent] -> Character
updateCharacter c xs = 
  let updatedCharacter = updateCharacterPosition xs c
      updatedCharacter' = if jumping updatedCharacter then jumpLogic updatedCharacter else gravityLogic updatedCharacter
  in updatedCharacter'
  


appLoop :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Renderer -> AssetMap SDL.Texture -> Character -> m Bool
appLoop window screen renderer assets character = do
  xs <- mapEventsToIntents <$> SDL.pollEvents

  -- frame start
  frameStart <- SDL.ticks

  let shouldQuit = Quit `elem` xs
  if shouldQuit
      then pure False
      else do
        isRightPressed <- liftIO $ isKeyPressed SDL.ScancodeRight
        isLeftPressed <- liftIO $ isKeyPressed SDL.ScancodeLeft
        let xs' = xs ++ [if isRightPressed then Render Right else if isLeftPressed then Render Left else NotImplemented]
        let character' = updateCharacter character xs'
        renderCharacter renderer assets character'

        SDL.delay 16
        appLoop window screen renderer assets character'
        
gravityLogic :: Character -> Character
gravityLogic c@Character { jumping = False, yPos = y, gravity = g }
  | isGround c = c
  | otherwise = c { yPos = y + g }
gravityLogic c = c

isGround :: Character -> Bool
isGround c@Character { yPos = y, yVelocity = vy, jumpHeight = jh } = y - vy - jh  >= 250  -- Altura do chão


jumpLogic :: Character -> Character
jumpLogic c@Character { jumping = False } = c
jumpLogic c@Character { jumping = True, yVelocity = 0 } = c { jumping = False }
jumpLogic c@Character { jumping = True, yVelocity = vy, xVelocity = vx, jumpHeight = jh } =
    if not (isGround c)
        then c { yVelocity = vy - gravity c, xVelocity = vx, yPos = yPos c - vy - jh, jumpHeight = jh - 1, xPos = xPos c + xVelocity c }  -- Atualiza a posição horizontal
        else c { jumping = False, yVelocity = 0, xVelocity = 0, jumpHeight = 0, xPos = xPos c + xVelocity c }  -- Atualiza a posição horizontal

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Oiram Epoosh Game" SDL.defaultWindow
  SDL.showWindow window
  screen <- SDL.getWindowSurface window

  SDL.surfaceFillRect screen Nothing (SDL.V4 maxBound maxBound maxBound maxBound)
  SDL.updateWindowSurface window

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  assets <- mapM (SDLImage.loadTexture renderer) surfacePaths
  
  SDL.copy renderer (backGround assets) Nothing Nothing
  let positionInitial = SDL.Rectangle (SDL.P (SDL.V2 50 50)) (SDL.V2 50 50)
  SDL.copy renderer (maskDude assets) Nothing (Just positionInitial)
  SDL.present renderer

  appLoop window screen renderer assets (Character 250 250 False 0 0 0 2)

  SDL.delay 100

  SDL.destroyWindow window
  SDL.quit
