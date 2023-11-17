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

data Character = Character { xPos :: Int, yPos :: Int }

updateCharacterPosition :: [Intent] -> Character -> Character
updateCharacterPosition directions character =
    foldl (\acc dir -> case dir of
                Render Left  -> acc { xPos = xPos acc - 5 }  -- Move left
                Render Right -> acc { xPos = xPos acc + 5 }  -- Move right
                Render Up    -> acc { yPos = yPos acc - 5 }  -- Move up
                Render Down  -> acc { yPos = yPos acc + 5 }  -- Move down
                _     -> acc
          ) character directions


appLoop :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Renderer -> AssetMap SDL.Texture -> Character -> m Bool
appLoop window screen renderer assets character = do
  xs <- mapEventsToIntents <$> SDL.pollEvents

  -- frame start
  frameStart <- SDL.ticks

  let shouldQuit = Quit `elem` xs
  if shouldQuit
      then pure False
      else do
        let updatedCharacter = updateCharacterPosition [(Render Right)] character
            positionInitial = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPos updatedCharacter) (fromIntegral $ yPos updatedCharacter))) (SDL.V2 50 50)
        SDL.copy renderer (backGround assets) Nothing Nothing
        SDL.copy renderer (maskDude assets) Nothing (Just positionInitial)
        SDL.present renderer

        -- frame end
        frameEnd <- SDL.ticks

        -- frame cap
        let frameTime = frameEnd - frameStart
        when (frameDelay > frameTime) $ SDL.delay (frameDelay - frameTime)

        appLoop window screen renderer assets updatedCharacter
        pure True


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


  -- asset surface convert to texture
  -- backgroundTexture <- SDL.createTextureFromSurface renderer (backGround assets)
  -- maskDudeTexure <- SDL.createTextureFromSurface renderer (maskDude assets)

  -- Load texture
  SDL.copy renderer (backGround assets) Nothing Nothing
  let positionInitial = SDL.Rectangle (SDL.P (SDL.V2 100 100)) (SDL.V2 50 50)
  SDL.copy renderer (maskDude assets) Nothing (Just positionInitial)
  SDL.present renderer

  -- Tela inicial

  -- Loop principal
  appLoop window screen renderer assets (Character 100 100)


  -- Tela final

  -- mapM_ SDL.freeSurface assets
  -- SDL.freeSurface screen
  SDL.destroyWindow window
  SDL.quit

