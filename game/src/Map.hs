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
  getMapMaxX,
  getMapMaxY,
  getMapMinX,
  getMapMinY
) where

import Foreign.C.Types (CInt)
import qualified SDL
import GameRectangle as GR

data BlockType = Ground | Land | Brick | NonCollisionBlock deriving (Eq, Show)

data Block = Block
  { 
    blockType :: BlockType,
    rectangle :: SDL.Rectangle CInt
  }
  deriving (Eq, Show)

createBlock :: BlockType -> Int -> Int -> Block
createBlock bt x y = Block bt (GR.createRectangle x y 50 50)

blockForSymbol :: Char -> BlockType
blockForSymbol '*' = Brick
blockForSymbol '#' = Ground
blockForSymbol '$' = Land
blockForSymbol '.' = NonCollisionBlock
blockForSymbol _   = NonCollisionBlock -- Default to NonCollisionBlock for unknown symbols


-- Map array * = Brick, # = Ground, $ = Land, . = NonCollisionBlock
mapShape :: [[Char]]
mapShape = 
  [ "**************************************",
    "*...................................**",
    "*...................................**",
    "*...............................******",
    "*.........................****......**",
    "*...............#...................**",
    "*..............#....****............**",
    "*.............#.....................**",
    "############.##########################",
    "$$$$$$$$$$$$.$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$.$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
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


getCollisionBlockRectangles :: [Block] -> [SDL.Rectangle CInt]
getCollisionBlockRectangles = map rectangle . getCollisionBlocks

getMapMaxX :: [Block] -> Int
getMapMaxX = maximum . map (GR.getRectangleX . rectangle)

getMapMaxY :: [Block] -> Int
getMapMaxY = maximum . map (GR.getRectangleY . rectangle)

getMapMinX :: [Block] -> Int
getMapMinX = minimum . map (GR.getRectangleX . rectangle)

getMapMinY :: [Block] -> Int
getMapMinY = minimum . map (GR.getRectangleY . rectangle)


