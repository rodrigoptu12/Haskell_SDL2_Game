module Map(
  mapShape,
  BlockType (..),
  Block (..),
  createBlock,
  blockForSymbol,
  createMapFromShape,
  getCollisionBlocks,
  getCollisionBlockRectangles,
  getCollisionGroundBlocks,
  getCollisionAppleBlocks,
  getBlocksAroundCharacter,
  getMapMaxX,
  getMapMaxY,
  getMapMinX,
  getMapMinY,
  removeAppleBlock,
  getRectangleEnemy
) where

import Foreign.C.Types (CInt)
import qualified SDL
import GameRectangle as GR

data BlockType = Ground | Land | Brick | Apple | NonCollisionBlock | Background | Enemy  deriving (Eq, Show)

data Block = Block
  { 
    blockType :: BlockType,
    rectangle :: SDL.Rectangle CInt
  }
  deriving (Eq, Show)

createBlock :: BlockType -> Int -> Int -> Block
createBlock bt x y 
  | bt == Apple = Block bt (GR.createRectangle x y 20 20)
  | otherwise = Block bt (GR.createRectangle x y 50 50) 

blockForSymbol :: Char -> BlockType
blockForSymbol '*' = Brick
blockForSymbol '#' = Ground
blockForSymbol '$' = Land
blockForSymbol '.' = NonCollisionBlock
blockForSymbol 'A' = Apple
blockForSymbol 'I' = Enemy
blockForSymbol _   = NonCollisionBlock -- Default to NonCollisionBlock for unknown symbols


-- Map array * = Brick, # = Ground, $ = Land, . = NonCollisionBlock
mapShape :: [[Char]]
-- mapShape = 
--   [ "**************************************",
--     "*...................................**",
--     "*......................A............**",
--     "*...............................******",
--     "*...................A.....****..A...**",
--     "*..............A#............A......**",
--     "*..............#....****............**",
--     "*.A...A.......#.....A.......A....A..**",
--     "############.##########################",
--     "$$$$$$$$$$$$.$$$$$$$$$$$$$$$$$$$$$$$$$$",
--     "$$$$$$$$$$$$.$$$$$$$$$$$$$$$$$$$$$$$$$$",
--     "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
--     "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
--     "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
--     "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
--   ]
mapShape =
  [ ".......................................",
    "#########....................#####.....",
    "*********######A..I..A#######*****.....",
    ".........******#######*******........A.",
    "..AA...........*******.......A.........",
    "..AA.........................####......",
    "##...........................****..####",
    "**#######.A........................****",
    "..*******#######..#..................A.",
    ".........*******....#######....I.....A.",
    ".................A..*******############",
    "...........................************",
    ".....................................AA",
    "..............I....A.................A.",
    "...........A########................###",
    "......A#####********######...I...###***",
    "...####*****........******#######***...",
    "...****...........A.......*******......",
    "..............A...I......A.............",
    "#######################################"
  ]
createBlockForColumn :: Int -> (Int, Char) -> [Block]
createBlockForColumn rowIndex (colIndex, symbol) =
  let x = colIndex * 50
      y = rowIndex * 50
      blockType' = blockForSymbol symbol
  in [createBlock blockType' x y]


createBlocksForRow :: Int -> [Char] -> [Block]
createBlocksForRow rowIndex row =
  let indexedRow = zip [0..] row
  in concatMap (createBlockForColumn rowIndex) indexedRow

createMapFromShape :: [[Char]] -> [Block]
createMapFromShape mapShape' = concatMap createBlocksForRowIndexed mapShapeWithIndex
  where
    createBlocksForRowIndexed :: (Int, [Char]) -> [Block]
    createBlocksForRowIndexed (rowIndex, row) = createBlocksForRow rowIndex row

    mapShapeWithIndex = zip [0..] mapShape'

getCollisionBlocks :: [Block] -> [Block]
getCollisionBlocks = filter (\b -> blockType b /= NonCollisionBlock)

getCollisionGroundBlocks :: [Block] -> [Block]
getCollisionGroundBlocks  = filter (\b -> blockType b == Ground) . getCollisionBlocks

getCollisionAppleBlocks :: [Block] -> [Block]
getCollisionAppleBlocks  = filter (\b -> blockType b == Apple) . getCollisionBlocks

getCollisionBlockRectangles :: [Block] -> [SDL.Rectangle CInt]
getCollisionBlockRectangles = map rectangle . getCollisionBlocks

getBlocksAroundCharacter :: SDL.Rectangle CInt -> [Block] -> [Block]
getBlocksAroundCharacter characterRectangle blocks = filter (\b -> GR.isNear characterRectangle (rectangle b)) blocks

getRectangleEnemy :: [Block] -> [SDL.Rectangle CInt]
getRectangleEnemy = do
  let getRectangle = map rectangle . getCollisionBlocks
  let getEnemy = filter (\b -> blockType b == Enemy) . getCollisionBlocks
  getRectangle . getEnemy
  
  
  --filter (\b -> blockType b == Enemy) . getCollisionBlocks . getRectangle



getRectangle:: [Block] -> [SDL.Rectangle CInt]
getRectangle = map rectangle . getCollisionBlocks


getMapMaxX :: [Block] -> Int
getMapMaxX = maximum . map (GR.getRectangleX . rectangle)

getMapMaxY :: [Block] -> Int
getMapMaxY = maximum . map (GR.getRectangleY . rectangle)

getMapMinX :: [Block] -> Int
getMapMinX = minimum . map (GR.getRectangleX . rectangle)

getMapMinY :: [Block] -> Int
getMapMinY = minimum . map (GR.getRectangleY . rectangle)

removeAppleBlock :: SDL.Rectangle CInt -> [Block] -> [Block]
removeAppleBlock block blocks = map transformBlock blocks
  where
    transformBlock b
      | rectangle b == block = b { blockType = NonCollisionBlock, rectangle = transformRectangle (rectangle b) }
      | otherwise = b

-- Função auxiliar para transformar o retângulo
transformRectangle :: SDL.Rectangle CInt -> SDL.Rectangle CInt
transformRectangle r = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 50 50)
  where
    x = fromIntegral $ getRectangleX r
    y = fromIntegral $ getRectangleY r




