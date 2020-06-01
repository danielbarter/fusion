{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (peek)
import Data.Vector
import Crypto.Hash.MD5 (hash)
import Data.Map.Strict hiding (size)
import Data.ByteString
import Data.ByteString.Internal (ByteString(..))
import qualified Data.Vector.Storable as Storable
import Codec.Picture

{-
boundary pattern for tiles
   1
  2 0
   3
this is the order of the boundary labels in manifest.csv
-}


data Tile = Tile
  { svg :: String
  , boundary :: (Int,Int,Int,Int)
  }

-- we refer to tiles by their index in the tile store
data TileStore = TileStore
  { tiles :: Vector Tile
  , vacuumTile :: Int -- usually 0
  }

{-
boundary pattern for macro tiles

  3 2
 4   1
 5   0
  6 7

tile pattern for macro tiles

  2 1
  3 0
-}

type MacroBoundary = (Int,Int,Int,Int,Int,Int,Int,Int)
type MacroTile = (Int,Int,Int,Int)
type LocalRelation = Vector MacroTile
type LocalRelations = Map MacroBoundary LocalRelation

data Options = Options
  { seed :: ByteString
  , numberOfRows :: Int
  , numberOfColumns :: Int
  , rowCutoff :: Int
  , columnCutoff :: Int
  , tileWidth :: Int
  , tileHeight :: Int
  }

foreign import ccall "&freeTeaLeaf" freeTeaLeafC :: FunPtr (Ptr Word8 -> IO ())
foreign import ccall "generateTeaLeaf" generateTeaLeafC ::
  Int -> Int -> Int -> Int -> Int -> IO (Ptr Word8)


generateTeaLeaf :: Options -> IO (Image Word8)
generateTeaLeaf Options{..} = do
  let size = numberOfRows * numberOfColumns
      PS hashPtr _ _ = ( hash seed )
  seedNum <- withForeignPtr ( castForeignPtr hashPtr ) peek
  ptr <- generateTeaLeafC seedNum
    numberOfRows numberOfColumns rowCutoff columnCutoff
  foreignPtr <- newForeignPtr freeTeaLeafC ptr
  vector <- Storable.freeze $ Storable.MVector size foreignPtr
  return $ Image numberOfColumns numberOfRows vector




main :: IO ()
main = generateTeaLeaf (Options "anna" 1000 200 20 3 101 101 ) >>= ( writePng "tealeaf.png" )

