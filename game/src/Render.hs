module Render (
  drawMap
) where

import Map
import SDL
import Assets
import Control.Monad.IO.Class (MonadIO)

drawMap :: (MonadIO m) => SDL.Renderer -> [Block] -> AssetMap SDL.Texture -> m ()
drawMap renderer blocks assets = 
  let drawBlock block = do
        let texture = case blockType block of
              Ground -> ground assets
              Land -> land assets
              Brick -> brick assets
              NonCollisionBlock -> backGround assets
        copy renderer texture Nothing (Just $ rectangle block)
  in mapM_ drawBlock blocks

