{-# LANGUAGE OverloadedStrings #-}

module GameState (
  GameState (..),
  initialState
) where

import qualified SDL
import qualified SDL.Image as SDLImage


import Control.Monad.State (StateT (..), evalStateT, get, modify, put)

-- import Control.Monad (mapM)

import Character
import Enemy
import Assets
import Map

data GameState = GameState
  { 
    window :: SDL.Window,
    renderer :: SDL.Renderer,
    assets :: AssetMap SDL.Texture,
    character :: Character,
    enemy :: Enemy,
    gameMap :: [Block]
  }

initialState :: IO GameState
initialState = do
  SDL.initializeAll
  w <- SDL.createWindow "Oiram Epoosh Game" SDL.defaultWindow
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  assets' <- mapM (SDLImage.loadTexture r) surfacePaths

  return $ GameState {
    window = w,
    renderer = r,
    assets = assets',
    character = createCharacter 350 317 False 0 0 0 2,
    enemy = createEnemy 250 317 False 0 0 2 2,
    gameMap = createMapFromShape mapShape
  }