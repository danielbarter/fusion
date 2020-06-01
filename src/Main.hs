{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (peek)
import qualified Data.Vector as V
import Crypto.Hash.MD5 (hash)
import Data.Map.Strict hiding (size)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Internal (ByteString(..))
import Data.Csv ((.:))
import qualified Data.Csv as Csv
import qualified Data.Vector.Storable as Storable
import System.Environment (getArgs)
import System.Directory (setCurrentDirectory)
import qualified Codec.Picture as JP

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
  { tiles :: V.Vector Tile
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
type LocalRelation = V.Vector MacroTile
type LocalRelations = Map MacroBoundary LocalRelation

data Options = Options
  { seed :: ByteString
  , numberOfRows :: Int
  , numberOfColumns :: Int
  , rowCutOff :: Int
  , columnCutOff :: Int
  , tileWidth :: Int
  , tileHeight :: Int
  } deriving (Show)

instance Csv.FromNamedRecord Options where
  parseNamedRecord m = Options <$>
    m .: "seed" <*>
    m .: "numberOfRows" <*>
    m .: "numberOfColumns" <*>
    m .: "rowCutOff" <*>
    m .: "columnCutOff" <*>
    m .: "tileWidth" <*>
    m .: "tileHeight"

foreign import ccall "&freeTeaLeaf" freeTeaLeafC :: FunPtr (Ptr Word8 -> IO ())
foreign import ccall "generateTeaLeaf" generateTeaLeafC ::
  Int -> Int -> Int -> Int -> Int -> IO (Ptr Word8)


generateTeaLeaf :: Options -> IO (JP.Image Word8)
generateTeaLeaf Options{..} = do
  let size = numberOfRows * numberOfColumns
      PS hashPtr _ _ = ( hash seed )
  seedNum <- withForeignPtr ( castForeignPtr hashPtr ) peek
  ptr <- generateTeaLeafC seedNum
    numberOfRows numberOfColumns rowCutOff columnCutOff
  foreignPtr <- newForeignPtr freeTeaLeafC ptr
  vector <- Storable.freeze $ Storable.MVector size foreignPtr
  return $ JP.Image numberOfColumns numberOfRows vector




main :: IO ()
main = do
  args <- getArgs
  if Prelude.length (args) /= 1
    then Prelude.putStrLn "expecting single argument: tile directory"
    else do
      let folder = Prelude.head args
      setCurrentDirectory folder
      optionsByteString <- L.readFile "options.csv"
      let eitherOptions =
            Csv.decodeByName optionsByteString
      case eitherOptions of
        Left err -> putStrLn $ "error parsing options.csv: " <> err
        Right (_,optionsVector) -> do
          let options = V.head optionsVector
          teaLeaf <- generateTeaLeaf options
          JP.writePng ("tealeaf_" <> (unpack $ seed options) <> ".png") teaLeaf

