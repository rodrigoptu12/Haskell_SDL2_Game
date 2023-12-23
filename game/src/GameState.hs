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
import qualified SDL.Font as Font
import FontGame
import GameRectangle
data GameState = GameState
  { 
    window :: SDL.Window,
    renderer :: SDL.Renderer,
    assets :: AssetMap SDL.Texture,
    character :: CD.Character,
    gameMap :: [Block],
    font :: Font.Font,
    enemys :: [Enemy],
    enemysMaps :: [[Enemy]],
    maps:: [[Block]],
    nextMap :: Int,
    hitEnemy :: Bool
  }

initialState :: IO GameState
initialState = do
  SDL.initializeAll
  Font.initialize
  -- 800 x 600
  w <- SDL.createWindow "Oiram Epoosh Game"  SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 800 600 }
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  assets' <- mapM (SDLImage.loadTexture r) surfacePaths
  font <- loadFont "./assets2try/fast99.ttf" 24
  let map' = map (\b -> createMapFromShape b)  mapShape

  let enemysRects = map (\b -> getRectangleEnemy b) map'
  let enemys' = map (\rects -> map (\rect -> createEnemy (getRectangleX rect) (getRectangleY rect) False 0 0 2 2 rect False 0) rects) enemysRects

  return $ GameState {
    window = w,
    renderer = r,
    assets = assets',
    character = createCharacter 00 10 False 10 10 1 3 False False False False 0, -- 1800 900
    -- enemy = createEnemy 250 317 False 0 0 2 2,  
    gameMap = head map', -- [Block]
    font = font,
    enemys = head enemys', -- [Enemy]
    enemysMaps = enemys', -- [[Enemy]]
    maps = map', -- [[Block]]
    nextMap = 1,
    hitEnemy = False
  }

