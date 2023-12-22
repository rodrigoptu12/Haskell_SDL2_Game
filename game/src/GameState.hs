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
    -- enemy :: Enemy,
    gameMap :: [Block],
    font :: Font.Font,
    enemys :: [Enemy]
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
  let map' =  createMapFromShape mapShape
  -- getEnemyBlocks <- getCollisionEnemyBlocks map
  let enemysRects =  getRectangleEnemy map'
  -- createEnemy :: Int -> Int -> Bool -> Int -> Int -> Int -> Int -> SDL.Rectangle CInt -> Enemy
  -- createEnemy x y j jh yv xv g rect= Enemy x y j jh yv xv g rect
  -- usar enemysBlocks para criar um array de  enemys
  -- let x = getRectangleX (head enemysBlocks)
  -- let y = getRectangleY (head enemysBlocks)
  --     enemys = [createEnemy x y False 0 0 2 2 (head enemysBlocks)]
  -- let enemys' = map (\rect -> createEnemy (getRectangleX rect) (getRectangleY rect) False 0 0 2 2 rect) enemysBlocks
  let enemys' = map (\b -> createEnemy (getRectangleX b) (getRectangleY b) False 0 0 2 2 b False 0) enemysRects


  return $ GameState {
    window = w,
    renderer = r,
    assets = assets',
    character = createCharacter 10 10 False 10 10 1 3 False False False False 0,
    -- enemy = createEnemy 250 317 False 0 0 2 2,  
    gameMap = map',
    font = font,
    enemys = enemys'
  }

