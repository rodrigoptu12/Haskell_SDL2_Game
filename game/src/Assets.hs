{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Assets (
  surfacePaths,
  AssetMap(..)
) where
    


data AssetMap a = AssetMap
  { maskDude  :: a
  , backGround :: a
  , ground :: a
  , land :: a
  , pinkMan :: a
  , brick :: a
  , apple :: a
  , enemy :: a
  , virtualGuy:: a
  } deriving (Foldable, Traversable, Functor)

surfacePaths :: AssetMap FilePath
surfacePaths =
  AssetMap
    { maskDude = "./assets/Main Characters/Mask Dude/Jump (32x32).png",
      backGround = "./assets/Background/Blue.png",
      ground = "./assets/Terrain/ground.png",
      land = "./assets/Terrain/land.png",
      pinkMan = "./assets/Main Characters/Pink Man/Jump (32x32).png",
      brick = "./assets/Terrain/tijolo.png",
      apple = "./assets/Items/Fruits/Apple.png",
      enemy = "./assets/Main Characters/Ninja Frog/Jump (32x32).png",
      virtualGuy = "./assets/Main Characters/Virtual Guy/Jump (32x32).png"
    }