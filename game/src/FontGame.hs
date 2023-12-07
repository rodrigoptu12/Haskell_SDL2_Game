module FontGame (
    loadFont,
    renderText,
    drawTexture
) where

import qualified SDL.Font as Font
import qualified SDL.Vect as Vect
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word8)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as CS
import Foreign.C.String (withCString)
import qualified SDL



-- Função para carregar uma fonte
loadFont :: MonadIO m => FilePath -> Int -> m Font.Font
loadFont path size = liftIO $ Font.load path size

renderText :: MonadIO m => Font.Font -> String -> SDL.V4 Word8 -> SDL.Renderer -> m SDL.Texture
renderText font text color renderer = liftIO $ do
  surface <- Font.blended font color (Text.pack text)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return texture

drawTexture :: SDL.Renderer -> SDL.Texture -> IO ()
drawTexture renderer texture = do
  let destRect = SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 100 50)
  SDL.copy renderer texture Nothing (Just destRect)


