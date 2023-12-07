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
appLoop :: GameState -> IO ()
appLoop state = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  if quit
    then return ()
    else do

        let (GameState window renderer assets character enemy gameMap) = state
        let movement = E.sdlEventsToGameEvents events character
        -- ----------------
        let getAppleBlocks = getCollisionAppleBlocks gameMap
        let getAppleRectangles = getCollisionBlockRectangles getAppleBlocks
        let hasAppleCollided = GR.hasCollidedWithApple (C.rectangle character)  getAppleRectangles 
        -- hasAppleCollided = (Bool, SDL.Rectangle CInt)
        -- se colidiu com a maçã então remove a maçã 
        let gameMap' = if (fst hasAppleCollided) then removeAppleBlock (snd hasAppleCollided) gameMap else gameMap



        drawMap renderer gameMap assets
        let character' = C.updateCharacterWithEvents movement gameMap' 
        drawCharacter renderer character'  (maskDude assets)

        SDL.present renderer
        SDL.delay 41
        appLoop state {character = character', gameMap = gameMap'}

game :: IO ()
game = do
  state <- initialState

  -- print map X and Y
  putStrLn $ "Map Max X: " ++ show (getMapMaxX $ gameMap state)
  putStrLn $ "Map Max Y: " ++ show (getMapMaxY $ gameMap state)

  appLoop state

  SDL.destroyWindow (window state)
  SDL.quit
