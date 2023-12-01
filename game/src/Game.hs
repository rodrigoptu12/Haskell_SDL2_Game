module Game(
  game
) where

import qualified SDL
import qualified SDL.Image
 
import Control.Monad.State (StateT (..), evalStateT, get, modify, put)

import GameState 
import Render

appLoop :: StateT GameState IO ()
appLoop = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  if quit
    then return ()
    else do
        GameState { window = window, renderer = renderer, assets = assets, character = character, enemy = enemy, gameMap = gameMap } <- get

        drawMap renderer gameMap assets

        SDL.present renderer
        SDL.delay 16
        appLoop 

game :: IO ()
game = do
  state <- initialState
  
  evalStateT appLoop state

  SDL.destroyWindow (window state)
  SDL.quit
