module Events (
    GameEvent(..),
    sdlEventsToGameEvents,
) where

import qualified SDL


data GameEvent = LeftPressed | RightPressed | UpPressed | DownPressed | NotImplemented |Quit deriving (Eq, Show)


sdlEventsToGameEvents :: [SDL.Event] -> [GameEvent]
sdlEventsToGameEvents = removeNotImplemented . map sdlEventToGameEvent
  
  
-- sdlEventToGameEvent :: SDL.Event -> GameEvent
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeLeft _)))) = LeftPressed
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeRight _)))) = RightPressed
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeUp _)))) = UpPressed
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeDown _)))) = DownPressed
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeQ _)))) = Quit
-- sdlEventToGameEvent _ = NotImplemented

sdlEventToGameEvent :: SDL.Event -> GameEvent
sdlEventToGameEvent (SDL.Event _ payload) = case payload of
    SDL.KeyboardEvent keyboardEvent -> case (SDL.keyboardEventKeyMotion keyboardEvent, SDL.keysymScancode (SDL.keyboardEventKeysym keyboardEvent)) of
        (SDL.Pressed, SDL.ScancodeLeft) -> LeftPressed
        (SDL.Pressed, SDL.ScancodeRight) -> RightPressed
        (SDL.Pressed, SDL.ScancodeUp) -> UpPressed
        (SDL.Pressed, SDL.ScancodeDown) -> DownPressed
        (SDL.Pressed, SDL.ScancodeQ) -> Quit
        (SDL.Pressed, _) -> NotImplemented
        (SDL.Released, _) -> NotImplemented
    SDL.QuitEvent -> Quit
    _ -> NotImplemented


removeNotImplemented :: [GameEvent] -> [GameEvent]
removeNotImplemented = filter (/= NotImplemented)

-- sdlEventToGameEvent :: [SDL.Event] -> [GameEvent]
-- sdlEventToGameEvent = undefined

