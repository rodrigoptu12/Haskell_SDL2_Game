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
-- unsafePerformIO
import System.IO.Unsafe (unsafePerformIO)
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

type MapRect = [Block]
data Block = Block
  { blockRect :: SDL.Rectangle CInt
  , blockType :: BlockType
  }
data BlockType
  = Ground
  | Land
  | Tijolo
  | Sky
  deriving (Show, Eq)

data GameState = GameState
  { window :: SDL.Window,
    renderer :: SDL.Renderer,
    assets :: AssetMap SDL.Texture,
    character :: Character,
    enemy :: Enemy,
    counter :: Int,
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

initializeMapRect :: Mapa -> IO [[Block]]
initializeMapRect mapa = do
  let mapa' = zip [0..] mapa
  forM mapa' $ \(y, linha) ->
    forM (zip [0..] linha) $ \(x, char) ->
      case char of
        '#' -> return $ Block (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ x * 46) (fromIntegral $ y * 46))) (SDL.V2 (fromIntegral 46) (fromIntegral 46))) Ground
        '$' -> return $ Block (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ x * 46) (fromIntegral $ y * 46))) (SDL.V2 (fromIntegral 46) (fromIntegral 46))) Land
        '*' -> return $ Block (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ x * 46) (fromIntegral $ y * 46))) (SDL.V2 (fromIntegral 46) (fromIntegral 46))) Tijolo
        _   -> return $ Block (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ x * 46) (fromIntegral $ y * 46))) (SDL.V2 (fromIntegral 46) (fromIntegral 46))) Sky

drawMap :: 
drawMap renderer mapRect assets = liftIO $ do
  mapRect' <- mapRect
  forM_ mapRect' $ \linha ->
    forM_ linha $ \block ->
      case blockType block of
        Ground -> SDL.copy renderer (ground assets) Nothing (Just $ blockRect block)
        Land -> SDL.copy renderer (land assets) Nothing (Just $ blockRect block)
        Tijolo -> SDL.copy renderer (tijoloWall assets) Nothing (Just $ blockRect block)
        Sky -> return ()

hasIntersectionMap :: IO [[Block]] -> SDL.Rectangle CInt -> IO Bool
hasIntersectionMap mapRect characterPosition = do
  mapRect' <- mapRect
  return $ any (\linha ->
              any (\block ->
                blockType block /= Sky && hasIntersection (blockRect block) characterPosition
              ) linha
            ) mapRect'


  
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

updateCharacterPosition :: [Intent] -> Character -> Character
updateCharacterPosition directions character =
    foldl (\acc dir -> case dir of
                Render Left  -> acc { xVelocity = -4, xPos = xPos acc - 4 }  -- Define a velocidade horizontal para a esquerda
                Render Right -> acc { xVelocity = 4, xPos = xPos acc + 4 }   -- Define a velocidade horizontal para a direita
                Render Up    -> if jumping acc then acc else acc { jumping = True, jumpHeight = 10, yVelocity = 13 }  -- Jump
                _     -> acc
          ) character directions

updateCharacter :: Character -> [Intent] -> Bool -> Character
updateCharacter c xs isGround = updateCharacterPosition xs c
      -- updatedCharacter' = if jumping updatedCharacter then jumpLogic isGround updatedCharacter  else updatedCharacter
  -- in updatedCharacter'
  

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


convertBool :: IO Bool -> Bool
convertBool bool = unsafePerformIO bool

appLoop :: StateT GameState IO ()
appLoop = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  -- se n 
  if quit
    then return ()
    else do
        GameState { window = window, renderer = renderer, assets = assets, character = character, enemy = enemy, counter = counter, exemploMapa =exemploMapa, mapRect = mapRect } <- get 
        let xs = mapEventsToIntents events
        frameStart <- SDL.ticks

        --  Update
        isRightPressed <- liftIO $ isKeyPressed SDL.ScancodeRight
        isLeftPressed <- liftIO $ isKeyPressed SDL.ScancodeLeft
        isUpPressed <- liftIO $ isKeyPressed SDL.ScancodeUp
        let xs' = xs ++ [if isRightPressed then Render Right else if isLeftPressed then Render Left else if isUpPressed then Render Up else NotImplemented]
        -- let isGround = hasCollision exemploMapa character
        let isIntersection = hasIntersectionMap mapRect (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPos character) (fromIntegral $ yPos character))) (SDL.V2 50 50))

        let characterGravity = gravityLogic character (convertBool isIntersection)

        let characterJumping = jumpLogic (convertBool isIntersection) characterGravity

        let character' = updateCharacter characterJumping xs' (convertBool isIntersection)
        renderCharacter renderer assets character'
        let characterPosition = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPos character') (fromIntegral $ yPos character'))) (SDL.V2 50 50)
              
    
        -- Colisão
        let enemy' = movingEnemy enemy
        
        -- let enemyPosition = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ xPosE enemy') (fromIntegral $ yPosE enemy'))) (SDL.V2 50 50)
        -- let intersect = hasIntersection characterPosition enemyPosition

        -- if intersect
        --   then do
        --     let character'' = character' { yPos = 244 }
        --     renderCharacter renderer assets character''
        --   else do
        --     renderCharacter renderer assets character'


        renderEnemy renderer assets enemy'
        -- renderGround renderer window assets 
        drawMap renderer (mapRect) assets
        --  Delay
        put $ GameState { window = window, renderer = renderer, assets = assets, character = character', enemy = enemy', counter = counter + 1, exemploMapa = exemploMapa, mapRect = mapRect }

        SDL.present renderer
        SDL.delay 16
        appLoop 

gravityLogic :: Character -> Bool ->Character
gravityLogic c isGround = do
  if not isGround then c { yPos = yPos c + gravity c  } else c



hasCollision :: Mapa -> Character -> Bool
hasCollision mapa character =
  any (\(y, row) -> any (\(x, tile) -> isBlock tile && collidesWith character (x, y)) (enumerate row)) (enumerate mapa)
  where
    enumerate :: [a] -> [(Int, a)]
    enumerate = zip [0..]

    collidesWith :: Character -> (Int, Int) -> Bool
    collidesWith c (blockX, blockY) =
      let blockSize = 46 -- Tamanho dos blocos, ajuste conforme necessário
          blockX' = fromIntegral blockX * blockSize
          blockY' = fromIntegral blockY * blockSize
      in xPos c < blockX' + blockSize
          && xPos c + characterWidth c > blockX'
          && yPos c < blockY' + blockSize
          && yPos c + characterHeight c > blockY'
          
    isBlock :: Char -> Bool
    isBlock tile = tile == '#' || tile == '$' || tile == '*'
    
    characterWidth :: Character -> Int
    characterWidth _ = 36 -- Largura do personagem, ajuste conforme necessário
    
    characterHeight :: Character -> Int
    characterHeight _ = 36 -- Altura do personagem, ajuste conforme necessário

jumpLogic :: Bool -> Character -> Character
jumpLogic isGround c@Character { jumping = False } = c
jumpLogic isGround c@Character { jumping = True, yVelocity = 0 } = c { jumping = False }
jumpLogic isGround c@Character { jumping = True, yVelocity = vy, xVelocity = vx, jumpHeight = jh } =
    if isGround
        then c { yVelocity = vy - gravity c, xVelocity = vx, yPos = yPos c - vy - jh, jumpHeight = jh - 1, xPos = xPos c + xVelocity c }  -- Atualiza a posição horizontal
        else c { jumping = False, yVelocity = 0, xVelocity = 0, jumpHeight = 0, xPos = xPos c + xVelocity c }  -- Atualiza a posição horizontal

-- jumpLogic isGround c@Character { jumping = False } = c
-- jumpLogic isGround c@Character { jumping = True, yVelocity = 0 } = c { jumping = False }
-- jumpLogic isGround c@Character { jumping = True, yVelocity = vy, xVelocity = vx, jumpHeight = jh } =
--   if not (isGround)
--     then c { yVelocity = vy - gravity c, xVelocity = vx, xPos = xPos c + xVelocity c, yPos = yPos c - vy - jh, jumpHeight = jh - 1 }
--     else c { jumping = False, yVelocity = 0, xVelocity = 0, jumpHeight = 0, xPos = xPos c + xVelocity c, yPos = yPos c + vy }


initialState :: IO GameState
initialState = do
  SDL.initializeAll
  w <- SDL.createWindow "Oiram Epoosh Game" SDL.defaultWindow
  SDL.showWindow w
  screen <- SDL.getWindowSurface w
  SDL.surfaceFillRect screen Nothing (SDL.V4 maxBound maxBound maxBound maxBound)
  SDL.updateWindowSurface w
  let map = mapInit

  let maprect = initializeMapRect map
  
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
        exemploMapa =  map,
        mapRect = maprect
      }

mapInit :: Mapa
mapInit = [ "*******************",
    "*...................",
    "*...................",
    "*...............................*****",
    "*.........................****",
    "*...................",
    "*...................****",
    "*...................",
    "####################",
    "$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$............$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
  ]

main :: IO ()
main = do

  SDL.initializeAll
  state <- initialState
  
  evalStateT appLoop state

  SDL.destroyWindow (window state)
  SDL.quit
