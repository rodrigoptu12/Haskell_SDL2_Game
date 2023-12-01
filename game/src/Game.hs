module Game(
  game
) where

import qualified SDL

import qualified Character as C
import GameState
import Assets
import Render
import Map
import Events



appLoop :: GameState -> IO ()
appLoop state = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  if quit
    then return ()
    else do

        let events' = sdlEventsToGameEvents events

        print events'

        let (GameState window renderer assets character enemy gameMap) = state

        drawMap renderer gameMap assets
        -- mover para o render
        
        let character' = C.updateCharacterWithEvents character events' gameMap 
        -- let character'' = C.gravityLogic character' gameMap
        -- print character'
        drawCharacter renderer character'  (maskDude assets)

        SDL.present renderer
        SDL.delay 16
        appLoop state {character = character'}

game :: IO ()
game = do
  state <- initialState

  -- print map X and Y
  putStrLn $ "Map Max X: " ++ show (getMapMaxX $ gameMap state)
  putStrLn $ "Map Max Y: " ++ show (getMapMaxY $ gameMap state)

  appLoop state

  SDL.destroyWindow (window state)
  SDL.quit
