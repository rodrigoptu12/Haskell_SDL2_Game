{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image as SDLImage
import Data.IORef
import Foreign.C.Types (CInt)
import Control.Monad (void, unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import SDL.Video.Renderer (Rectangle)
import Control.Monad.Extra    (loopM, whileM)
import Control.Monad.State
import  Prelude                hiding (Left, Right)
import Data.Maybe (mapMaybe)
import GHC.Word (Word32)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State (StateT (..), evalStateT, get, modify, put)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)

data Intent
  = Render Direction
  | Quit
  | NotImplemented
  deriving (Show, Eq)


data AssetMap a = AssetMap
  { maskDude  :: a
  , backGround :: a
  , ground :: a
  , land :: a
  , pinkMan :: a
  , tijoloWall :: a
  } deriving (Foldable, Traversable, Functor)




drawMap :: (MonadIO m) => SDL.Renderer -> Mapa -> AssetMap SDL.Texture -> m ()
drawMap renderer mapa assets= liftIO $ do
  let mapa' = zip [0..] mapa
  forM_ mapa' $ \(y, linha) ->
    forM_ (zip [0..] linha) $ \(x, char) ->
      case char of
        '#' -> do
          SDL.copy renderer (ground assets) Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (x * 46) (y * 46))) (SDL.V2 46 46))
        '$' -> do
          SDL.copy renderer (land assets) Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (x * 46) (y * 46))) (SDL.V2 46 46))
        '*' -> do
          SDL.copy renderer (tijoloWall assets) Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (x * 46) (y * 46))) (SDL.V2 46 46))
        _ -> return ()



data Character = Character
 { xPos :: Int
 , yPos :: Int
 , jumping :: Bool
 , jumpHeight :: Int
 , yVelocity :: Int
 , xVelocity :: Int
 , gravity :: Int
 }

data Enemy = Enemy
 { xPosE :: Int
 , yPosE :: Int
 , jumpingE :: Bool
 , jumpHeightE :: Int
 , yVelocityE :: Int
 , xVelocityE :: Int
 , gravityE :: Int
 }

type Mapa = [[Char]]

data GameState = GameState
  { window :: SDL.Window,
    renderer :: SDL.Renderer,
    assets :: AssetMap SDL.Texture,
    character :: Character,
    enemy :: Enemy,
    counter :: Int,
    exemploMapa :: Mapa
  }

surfacePaths :: AssetMap FilePath
surfacePaths = AssetMap
  {
    maskDude  = "./assets/Main Characters/Mask Dude/Jump (32x32).png",
    backGround = "./assets/Background/Blue.png",
    ground = "./assets/Terrain/ground.png",
    land = "./assets/Terrain/land.png",
    pinkMan = "./assets/Main Characters/Pink Man/Jump (32x32).png",
    tijoloWall = "./assets/Terrain/tijolo.png"
  }

-- define a 60 FPS constant
fps :: GHC.Word.Word32
fps = 60
frameDelay :: GHC.Word.Word32
frameDelay = 1000 `div` fps -- 1000 ms / 60 fps = 16.6666 ms

getKey :: SDL.KeyboardEventData -> Maybe Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =  Just $
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeUp     -> Render Up
    SDL.KeycodeDown   -> Render Down
    SDL.KeycodeLeft   -> Render Left
    SDL.KeycodeRight  -> Render Right
    _                 -> NotImplemented

mapEventsToIntents :: [SDL.Event] -> [Intent]
mapEventsToIntents = mapMaybe (payloadToIntent . SDL.eventPayload)
  where
    payloadToIntent :: SDL.EventPayload -> Maybe Intent
    payloadToIntent (SDL.KeyboardEvent k) = getKey k
    payloadToIntent SDL.QuitEvent         = Just Quit
    payloadToIntent _                     = Nothing

updateCharacterPosition :: [Intent] -> Character -> Character
updateCharacterPosition directions character =
    foldl (\acc dir -> case dir of
                Render Left  -> acc { xVelocity = -4, xPos = xPos acc - 4 }  -- Define a velocidade horizontal para a esquerda
                Render Right -> acc { xVelocity = 4, xPos = xPos acc + 4 }   -- Define a velocidade horizontal para a direita
                Render Up    -> if jumping acc then acc else acc { jumping = True, jumpHeight = 10, yVelocity = 13 }  -- Jump
                _     -> acc
          ) character directions

-- ground.png 46x46, multiplicapar para toda a tela
renderGround :: (MonadIO m) => SDL.Renderer -> SDL.Window -> AssetMap SDL.Texture -> m ()
renderGround renderer window assets = liftIO $ do
    let groundTexture = ground assets

    SDL.TextureInfo _format _access _width _height <- SDL.queryTexture groundTexture

    SDL.V2 windowWidth windowHeight  <- SDL.get (SDL.windowSize window)

    let repeatX = ceiling (fromIntegral windowWidth / fromIntegral _width :: Float)
        repeatY = ceiling (fromIntegral windowHeight / fromIntegral _height :: Float)

    forM_ [0..repeatX - 1] $ \x ->
        forM_ [0..repeatY - 1] $ \y ->
            SDL.copy renderer groundTexture Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (x * _width) (300))) (SDL.V2 _width _height))
    let landTexture = land assets

    forM_ [346, 392..windowHeight] $ \y ->
      forM_ [0..repeatX - 1] $ \x ->
        SDL.copy renderer landTexture Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (x * _width) y)) (SDL.V2 _width _height))

renderCharacter :: (MonadIO m) => SDL.Renderer -> AssetMap SDL.Texture -> Character -> m ()
renderCharacter renderer assets c = liftIO $ do
    let position = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPos c) (fromIntegral $ yPos c))) (SDL.V2 50 50)
    SDL.copy renderer (backGround assets) Nothing Nothing
    SDL.copy renderer (maskDude assets) Nothing (Just position)

renderEnemy :: (MonadIO m) => SDL.Renderer -> AssetMap SDL.Texture -> Enemy -> m ()
renderEnemy renderer assets c = liftIO $ do
    let position = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPosE c) (fromIntegral $ yPosE c))) (SDL.V2 50 50)
    SDL.copy renderer (pinkMan assets) Nothing (Just position)


isKeyPressed :: SDL.Scancode -> IO Bool
isKeyPressed scancode = do
  keyboardState <- SDL.getKeyboardState
  return $ keyboardState scancode

updateCharacter :: Character -> [Intent] -> Character
updateCharacter c xs = 
  let updatedCharacter = updateCharacterPosition xs c
      updatedCharacter' = if jumping updatedCharacter then jumpLogic updatedCharacter else gravityLogic updatedCharacter
  in updatedCharacter'
  

hasIntersection :: SDL.Rectangle CInt -> SDL.Rectangle CInt -> Bool
hasIntersection (SDL.Rectangle (SDL.P (SDL.V2 x1 y1)) (SDL.V2 w1 h1)) (SDL.Rectangle (SDL.P (SDL.V2 x2 y2)) (SDL.V2 w2 h2)) =
  let x1' = fromIntegral x1
      y1' = fromIntegral y1
      w1' = fromIntegral w1
      h1' = fromIntegral h1
      x2' = fromIntegral x2
      y2' = fromIntegral y2
      w2' = fromIntegral w2
      h2' = fromIntegral h2
  in x1' < x2' + w2' && x1' + w1' > x2' && y1' < y2' + h2' && y1' + h1' > y2'
 
movingEnemy :: Enemy -> Enemy
movingEnemy character 
  | xPosE character <= 300 = character { xVelocityE = 2, xPosE = xPosE character + 2 }
  | xPosE character >= 600 = character { xVelocityE = -2, xPosE = xPosE character - 2 }
  | xPosE character <= 600 && xPosE character >= 300 = character { xVelocityE = xVelocityE character, xPosE = xPosE character + xVelocityE character }

appLoop :: StateT GameState IO ()
appLoop = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  -- se n 
  if quit
    then return ()
    else do
        GameState { window = window, renderer = renderer, assets = assets, character = character, enemy = enemy, counter = counter, exemploMapa =exemploMapa } <- get 
        let xs = mapEventsToIntents events
        frameStart <- SDL.ticks

        --  Update
        isRightPressed <- liftIO $ isKeyPressed SDL.ScancodeRight
        isLeftPressed <- liftIO $ isKeyPressed SDL.ScancodeLeft
        isUpPressed <- liftIO $ isKeyPressed SDL.ScancodeUp
        let xs' = xs ++ [if isRightPressed then Render Right else if isLeftPressed then Render Left else if isUpPressed then Render Up else NotImplemented]


        let character' = updateCharacter character xs'
        renderCharacter renderer assets character'

        -- Colisão
        let enemy' = movingEnemy enemy
        let characterPosition = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPos character') (fromIntegral $ yPos character'))) (SDL.V2 50 50)
        let enemyPosition = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPosE enemy') (fromIntegral $ yPosE enemy'))) (SDL.V2 50 50)
        let intersect = hasIntersection characterPosition enemyPosition

        if intersect
          then do
            let character'' = character' { yPos = 244 }
            renderCharacter renderer assets character''
          else do
            renderCharacter renderer assets character'


        renderEnemy renderer assets enemy'
        -- renderGround renderer window assets 
        drawMap renderer exemploMapa assets

        --  Delay
        put $ GameState { window = window, renderer = renderer, assets = assets, character = character', enemy = enemy', counter = counter + 1, exemploMapa = exemploMapa }

        SDL.present renderer
        SDL.delay 16
        appLoop 
        
gravityLogic :: Character -> Character
gravityLogic c@Character { jumping = False, yPos = y, gravity = g }
  | isGround c = c
  | otherwise = c { yPos = y + g }
gravityLogic c = c


hasCollision :: Mapa -> Character -> Bool
hasCollision mapa character =
  any (\(y, row) -> any (\(x, tile) -> isBlock tile && collidesWith character (x, y)) (enumerate row)) (enumerate mapa)
  where
    enumerate :: [a] -> [(Int, a)]
    enumerate = zip [0..]

    collidesWith :: Character -> (Int, Int) -> Bool
    collidesWith c (blockX, blockY) =
      let blockX' = fromIntegral blockX * blockSize
          blockY' = fromIntegral blockY * blockSize
          blockSize = 50 -- Tamanho dos blocos, ajuste conforme necessário
      in xPos c < blockX' + blockSize
          && xPos c + characterWidth > blockX'
          && yPos c < blockY' + blockSize
          && yPos c + characterHeight > blockY'
          
    isBlock :: Char -> Bool
    isBlock tile = tile == '#'
    
    characterWidth = 50 -- Largura do personagem, ajuste conforme necessário
    characterHeight = 50 -- Altura do personagem, ajuste conforme necessário



isGround :: Character -> Bool
--  detecta se o personagem está no chão, se colidir com algum bloco
isGround c@Character { yPos = y, yVelocity = vy, jumpHeight = jh }  = y + vy + jh >= 317


jumpLogic :: Character -> Character
jumpLogic c@Character { jumping = False } = c
jumpLogic c@Character { jumping = True, yVelocity = 0 } = c { jumping = False }
jumpLogic c@Character { jumping = True, yVelocity = vy, xVelocity = vx, jumpHeight = jh } =
    if not (isGround c)
        then c { yVelocity = vy - gravity c, xVelocity = vx, yPos = yPos c - vy - jh, jumpHeight = jh - 1, xPos = xPos c + xVelocity c }  -- Atualiza a posição horizontal
        else c { jumping = False, yVelocity = 0, xVelocity = 0, jumpHeight = 0, xPos = xPos c + xVelocity c }  -- Atualiza a posição horizontal


initialState :: IO GameState
initialState = do
  SDL.initializeAll
  w <- SDL.createWindow "Oiram Epoosh Game" SDL.defaultWindow
  SDL.showWindow w
  screen <- SDL.getWindowSurface w
  SDL.surfaceFillRect screen Nothing (SDL.V4 maxBound maxBound maxBound maxBound)
  SDL.updateWindowSurface w
  -- map <- exemploMap
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  assets' <- mapM (SDLImage.loadTexture r) surfacePaths
  return $
    GameState
      { window = w,
        renderer = r,
        assets = assets',
        character = (Character 350 317 False 0 0 0 2),
        enemy = (Enemy 250 317 False 0 0 2 2),
        counter = 0,
        exemploMapa =  [ "*******************",
    "*...................",
    "*...................",
    "*...............................*****",
    "*.........................****",
    "*...................****",
    "*...................",
    "*...................",
    "####################",
    "$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$"
  ]
      }


main :: IO ()
main = do

  SDL.initializeAll
  state <- initialState
  
  evalStateT appLoop state

  SDL.destroyWindow (window state)
  SDL.quit
