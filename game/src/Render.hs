module Render (
  drawMap,
  drawCharacter
) where

import Map
import SDL
import SDL.Font

import Assets
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CInt)
import qualified Character as Character'
import qualified Map as Map'

--  font  ./assets2try/lazy.ttf


drawMap :: (MonadIO m) => SDL.Renderer -> [Block] -> AssetMap SDL.Texture -> m ()
drawMap renderer blocks assets = 
  let drawBlock block = do
        let texture = case blockType block of
              Ground -> ground assets
              Land -> land assets
              Brick -> brick assets
              NonCollisionBlock -> backGround assets
        copy renderer texture Nothing (Just $ Map'.rectangle block)
  in mapM_ drawBlock blocks

drawCharacter :: (MonadIO m) => SDL.Renderer -> Character'.Character -> SDL.Texture -> m ()
drawCharacter renderer character texture = copy renderer texture Nothing (Just $ Character'.rectangle character)