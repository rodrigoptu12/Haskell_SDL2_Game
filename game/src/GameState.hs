{-# LANGUAGE OverloadedStrings #-}

module GameState (
  GameState (..),
  initialState
) where

import qualified SDL
import qualified SDL.Image as SDLImage

import qualified CharacterData as CD
import Character
import Enemy
import Assets
import Map
-- import Events  
data GameState = GameState
  { 
    window :: SDL.Window,
    renderer :: SDL.Renderer,
    assets :: AssetMap SDL.Texture,
    character :: CD.Character,
    enemy :: Enemy,
    gameMap :: [Block]
  }

initialState :: IO GameState
initialState = do
  SDL.initializeAll
  -- 800 x 600
  w <- SDL.createWindow "Oiram Epoosh Game"  SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 800 600 }
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  assets' <- mapM (SDLImage.loadTexture r) surfacePaths

  return $ GameState {
    window = w,
    renderer = r,
    assets = assets',
    character = createCharacter 350 317 False 10 10 1 3 False False False False,
    enemy = createEnemy 250 317 False 0 0 2 2,
    gameMap = createMapFromShape mapShape
  }

