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
import FontGame
import Enemy
import qualified Enemy as E

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

        let (GameState window renderer assets character  gameMap font enemys enemysMaps maps nextMap hitEnemy ) = state


        -- verify position character 1800 900
        let characterX = C.xPos character
        let characterY = C.yPos character

        -- if character X collide with area betweenX (1790 - 1860) and Y (890 - 960) then change map
        let finish = if (characterX >= 1790 && characterX <= 1860 && characterY >= 890 && characterY <= 960) then True else False

        -- if finish and there is no more maps then show 'You Win' and finish game
        let finishGame = if finish && nextMap >= length maps then True else False

        if finishGame then do
          font' <- loadFont "./assets2try/fast99.ttf" 100
          textTexture <- renderText font' "You Win! Thanks for playing" (SDL.V4 255 255 255 255) renderer
          drawTextureGameEnd renderer textTexture
          SDL.present renderer
          SDL.delay 16
          appLoop state
        else do
          let gameMapCurrent = if finish then maps !! nextMap else gameMap
          let nextMap' = if finish then nextMap + 1 else nextMap
          let characterPos = if finish || hitEnemy  then character {
            C.xPos = 10,
            C.yPos = 10,
            C.score = 0,
            C.rectangle = GR.updateRectangleY (GR.updateRectangleX (C.rectangle character) 10) 10
            } else character


          ------- TEM QUE MOVER Apple --------
          let getAppleBlocks = getCollisionAppleBlocks gameMapCurrent
          let getAppleRectangles = getCollisionBlockRectangles getAppleBlocks
          let hasAppleCollided = GR.hasCollidedWithApple (C.rectangle characterPos)  getAppleRectangles
          let character' = if  fst hasAppleCollided then C.updateCharacterScore characterPos else characterPos
          let gameMap' = if fst hasAppleCollided then removeAppleBlock (snd hasAppleCollided) gameMapCurrent else gameMapCurrent
          -------------------------------------

          let movement = E.sdlEventsToGameEvents events character'
          drawMap renderer gameMap assets
          let character'' = C.updateCharacterWithEvents movement gameMap'
          drawCharacter renderer character''  (maskDude assets)

          ------- TEM QUE MOVER Score --------
          textTexture <- renderText font ("Score:    " ++ show (C.score character')) (SDL.V4 255 255 255 255) renderer
          drawTexture renderer textTexture
          ------------------------------------

          -- enemy --- 
          let enemys' = map updateEnemy enemys
          mapM_ (\enemy' -> drawEnemy renderer enemy' (pinkMan assets)) enemys'
          ------------------------------------

          -- Se colidir com inimigo então volta para o inicio - use enemys'
          let getEnemyRectangles = map (\enemy' -> E.rectangle enemy') enemys'
          
          let hitEnemy' = GR.hasCollidedWithAny (C.rectangle characterPos)  getEnemyRectangles

          SDL.present renderer
          SDL.delay 16
          appLoop state {character = character'', gameMap = gameMap', enemys = enemys', nextMap = nextMap', hitEnemy = hitEnemy'}


game :: IO ()
game = do
  state <- initialState

  -- print map X and Y
  -- putStrLn $ "Map Max X: " ++ show (getMapMaxX $ gameMap state)
  -- putStrLn $ "Map Max Y: " ++ show (getMapMaxY $ gameMap state)

  appLoop state

  SDL.destroyWindow (window state)
  SDL.quit
