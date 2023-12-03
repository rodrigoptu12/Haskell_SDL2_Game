module Events (
    GameEvent(..),
    sdlEventsToGameEvents,
) where

import qualified SDL
import CharacterData
data GameEvent = LeftPressed | RightPressed | UpPressed | DownPressed | NotImplemented |Quit deriving (Eq, Show)


-- sdlEventsToGameEvents :: [SDL.Event] -> [GameEvent]
-- sdlEventsToGameEvents = removeNotImplemented . map sdlEventToGameEvent
  
  
-- sdlEventToGameEvent :: SDL.Event -> GameEvent
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeLeft _)))) = LeftPressed
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeRight _)))) = RightPressed
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeUp _)))) = UpPressed
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeDown _)))) = DownPressed
-- sdlEventToGameEvent (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeQ _)))) = Quit
-- sdlEventToGameEvent _ = NotImplemented

-- sdlEventToGameEvent :: SDL.Event -> GameEvent
-- sdlEventToGameEvent (SDL.Event _ payload) = case payload of
--     SDL.KeyboardEvent keyboardEvent -> case (SDL.keyboardEventKeyMotion keyboardEvent, SDL.keysymScancode (SDL.keyboardEventKeysym keyboardEvent)) of
--         (SDL.Pressed, SDL.ScancodeLeft) -> LeftPressed
--         (SDL.Pressed, SDL.ScancodeRight) -> RightPressed
--         (SDL.Pressed, SDL.ScancodeUp) -> UpPressed
--         (SDL.Pressed, SDL.ScancodeDown) -> DownPressed
--         (SDL.Pressed, SDL.ScancodeQ) -> Quit
--         (SDL.Pressed, _) -> NotImplemented
--         (SDL.Released, _) -> NotImplemented
--     SDL.QuitEvent -> Quit
--     _ -> NotImplemented
-- sdlEventToGameEvent :: SDL.Event -> GameEvent
-- -- verificando se o evento é do teclado e se a tecla continua pressionada 
-- sdlEventToGameEvent event =
--     case SDL.eventPayload event of
--         SDL.KeyboardEvent keyboardEvent ->
--             case SDL.keyboardEventKeyMotion keyboardEvent of
--                 SDL.Pressed ->
--                     case SDL.keysymScancode (SDL.keyboardEventKeysym keyboardEvent) of
--                         SDL.ScancodeLeft -> LeftPressed
--                         SDL.ScancodeRight -> RightPressed
--                         SDL.ScancodeUp -> UpPressed
--                         SDL.ScancodeDown -> DownPressed
--                         SDL.ScancodeQ -> Quit
--                         _ -> NotImplemented
--                 SDL.Released -> NotImplemented
--         -- SDL.QuitEvent -> Quit
--         _ -> NotImplemented

-- data Character = Character
--   { xPos :: Int,
--     yPos :: Int,
--     jumping :: Bool,
--     jumpHeight :: Int,
--     yVelocity :: Int,
--     xVelocity :: Int,
--     gravity :: Int,
--     rectangle :: SDL.Rectangle CInt,
--     leftPressed :: Bool,
--     rightPressed :: Bool,
--     upPressed :: Bool,
--     downPressed :: Bool
--   }

sdlEventsToGameEvents :: [SDL.Event] -> Character -> Character 
-- verificando se o evento é do teclado e se a tecla continua pressionada
sdlEventsToGameEvents events character =
    case events of
        [] -> character
        (event:xs) ->
            case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                    case SDL.keyboardEventKeyMotion keyboardEvent of
                        SDL.Pressed ->
                            case SDL.keysymScancode (SDL.keyboardEventKeysym keyboardEvent) of
                                SDL.ScancodeLeft -> sdlEventsToGameEvents xs (character {leftPressed = True})
                                SDL.ScancodeRight -> sdlEventsToGameEvents xs (character {rightPressed = True})
                                SDL.ScancodeUp -> sdlEventsToGameEvents xs (character {upPressed = True})
                                SDL.ScancodeDown -> sdlEventsToGameEvents xs (character {downPressed = True})
                                SDL.ScancodeQ -> sdlEventsToGameEvents xs (character {leftPressed = True})
                                _ -> sdlEventsToGameEvents xs character
                        SDL.Released ->
                            case SDL.keysymScancode (SDL.keyboardEventKeysym keyboardEvent) of
                                SDL.ScancodeLeft -> sdlEventsToGameEvents xs (character {leftPressed = False})
                                SDL.ScancodeRight -> sdlEventsToGameEvents xs (character {rightPressed = False})
                                SDL.ScancodeUp -> sdlEventsToGameEvents xs (character {upPressed = False})
                                SDL.ScancodeDown -> sdlEventsToGameEvents xs (character {downPressed = False})
                                SDL.ScancodeQ -> sdlEventsToGameEvents xs (character {leftPressed = False})
                                _ -> sdlEventsToGameEvents xs character
                _ -> sdlEventsToGameEvents xs character
