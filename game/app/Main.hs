{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image as SDLImage
import Data.IORef
import Foreign.C.Types (CInt)
import Control.Monad (void, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import SDL.Video.Renderer (Rectangle)
import Control.Monad.Extra    (loopM, whileM)
import Control.Monad.State
import  Prelude                hiding (Left, Right)
import Data.Maybe (mapMaybe)

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
                Render Left  -> acc { xPos = xPos acc - 2 }  -- Move left
                Render Right -> acc { xPos = xPos acc + 2 }  -- Move right
                _     -> acc
          ) character directions



appLoop :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Renderer -> AssetMap SDL.Texture -> Character -> m Bool
appLoop window screen renderer assets character = do
  xs <- mapEventsToIntents <$> SDL.pollEvents

  let shouldQuit = Quit `elem` xs
  if shouldQuit
      then pure False
      else do
        let updatedCharacter = updateCharacterPosition xs character
            positionInitial = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPos updatedCharacter) (fromIntegral $ yPos updatedCharacter))) (SDL.V2 50 50)
        SDL.copy renderer (backGround assets) Nothing Nothing
        SDL.copy renderer (maskDude assets) Nothing (Just positionInitial)
        SDL.present renderer

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
  
  -- SDL.present renderer
  SDL.delay 1000

  -- mapM_ SDL.freeSurface assets
  -- SDL.freeSurface screen
  SDL.destroyWindow window
  SDL.quit

