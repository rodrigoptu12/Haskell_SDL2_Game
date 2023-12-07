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
import qualified GameRectangle as GR
--  font  ./assets2try/lazy.ttf


drawMap :: (MonadIO m) => SDL.Renderer -> [Block] -> AssetMap SDL.Texture -> m ()
drawMap renderer blocks assets = 
  let drawBlock block = do
        let texture = case blockType block of
              Ground -> ground assets
              Land -> land assets
              Brick -> brick assets
              Apple -> apple assets
              NonCollisionBlock -> backGround assets
        -- se texture == apple assets render apple e background
        if texture == apple assets then do
          -- create rectangle for background 50x50
          let appleTam = Map'.rectangle block
          let x = fromIntegral $ GR.getRectangleX appleTam
          let y = fromIntegral $ GR.getRectangleY appleTam
          let backTam = Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 50 50 )
          copy renderer (backGround assets) Nothing backTam
          copy renderer texture Nothing (Just appleTam) 

          
        else
          copy renderer texture Nothing (Just $ Map'.rectangle block)
  in mapM_ drawBlock blocks

drawCharacter :: (MonadIO m) => SDL.Renderer -> Character'.Character -> SDL.Texture -> m ()
drawCharacter renderer character texture = copy renderer texture Nothing (Just $ Character'.rectangle character)



