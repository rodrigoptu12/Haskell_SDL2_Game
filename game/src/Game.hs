module Game(
  game
) where

import qualified SDL

import qualified Character as C
import GameState
import Assets
import Render
import Map
import qualified Events as E
import qualified GameRectangle as GR



import qualified SDL.Font as Font
import qualified SDL.Vect as Vect
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word8)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as CS
import Foreign.C.String (withCString)


appLoop :: GameState -> IO ()
appLoop state = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  if quit
    then return ()
    else do

        let (GameState window renderer assets character enemy gameMap) = state
        ------- Apple --------
        let getAppleBlocks = getCollisionAppleBlocks gameMap
        let getAppleRectangles = getCollisionBlockRectangles getAppleBlocks
        let hasAppleCollided = GR.hasCollidedWithApple (C.rectangle character)  getAppleRectangles 
        let character' = if  (fst hasAppleCollided) then C.updateCharacterScore character else character
        let gameMap' = if (fst hasAppleCollided) then removeAppleBlock (snd hasAppleCollided) gameMap else gameMap
        print (C.score character')
        ----------------------

        font <- loadFont "./assets2try/fast99.ttf" 24

        -- textTexture <- renderText font "Pontuação" ++ show (C.pontuacao character) (SDL.V4 255 255 255 255) renderer
        textTexture <- renderText font ("Score:    " ++ show (C.score character')) (SDL.V4 255 255 255 255) renderer

        let movement = E.sdlEventsToGameEvents events character'

        drawMap renderer gameMap assets
        let character'' = C.updateCharacterWithEvents movement gameMap' 
        drawCharacter renderer character''  (maskDude assets)

        drawTexture renderer textTexture
        SDL.present renderer
        SDL.delay 41
        appLoop state {character = character'', gameMap = gameMap'}






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
  -- SDL.copy renderer texture Nothing Nothing














game :: IO ()
game = do
  state <- initialState
  
  -- print map X and Y
  putStrLn $ "Map Max X: " ++ show (getMapMaxX $ gameMap state)
  putStrLn $ "Map Max Y: " ++ show (getMapMaxY $ gameMap state)

  appLoop state

  SDL.destroyWindow (window state)
  SDL.quit
