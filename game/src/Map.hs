module Map(
  mapShape,
  BlockType (..),
  Block (..),
  createBlock,
  blockForSymbol,
  createMapFromShape,
  getCollisionBlocks,
  getCollisionBlockRectangles
) where

import Foreign.C.Types (CInt)
import qualified SDL

data BlockType = Ground | Land | Brick | NonCollisionBlock deriving (Eq, Show)

data Block = Block
  { 
    blockType :: BlockType,
    rectangle :: SDL.Rectangle CInt
  }
  deriving (Eq, Show)

createBlock :: BlockType -> Int -> Int -> Block
createBlock bt x y = Block bt (SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 46 46))

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
    "*...................................**",
    "*...................****............**",
    "*...................................**",
    "#######################################",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
    "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
  ]

createBlockForColumn :: Int -> (Int, Char) -> [Block]
createBlockForColumn rowIndex (colIndex, symbol) =
  let x = colIndex * 46
      y = rowIndex * 46
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

getCollisionBlockRectangles :: [Block] -> [SDL.Rectangle CInt]
getCollisionBlockRectangles = map rectangle . getCollisionBlocks

