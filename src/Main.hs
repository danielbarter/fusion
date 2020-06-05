{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (peek)
import qualified Data.Vector as V
import Crypto.Hash.MD5 (hash)
import qualified Data.Map.Strict as M
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Internal (ByteString(..))
import Data.Csv ((.:), (.!))
import qualified Data.Csv as Csv
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import System.Environment (getArgs)
import System.Directory (setCurrentDirectory)
import qualified Codec.Picture as JP
import System.Random (randomIO,mkStdGen,setStdGen)

{-
boundary pattern for tiles
   1
  2 0
   3
this is the order of the boundary labels in manifest.csv
-}


data Tile = Tile
  { svg :: String
  , right :: Int
  , top :: Int
  , left :: Int
  , bottom :: Int
  } deriving (Show)

instance Csv.FromRecord Tile where
  parseRecord m =
    Tile <$> m .! 0 <*> m .! 1 <*>  m .! 2 <*> m .! 3 <*> m .! 4



-- we refer to tiles by their index in the tile store
type TileStore = V.Vector Tile

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

data MacroBoundary = MacroBoundary
  { clock4 :: Int
  , clock2 :: Int
  , clock1 :: Int
  , clock11 :: Int
  , clock10 :: Int
  , clock8 :: Int
  , clock7 :: Int
  , clock5 :: Int
  } deriving (Eq, Ord, Show)


data MacroTile = MacroTile
  { bottomRight :: Int
  , topRight :: Int
  , topLeft :: Int
  , bottomLeft :: Int
  } deriving (Show)

macroBoundary :: TileStore -> MacroTile -> MacroBoundary
macroBoundary tiles MacroTile{..} =
  MacroBoundary
  { clock4  = right  $ tiles V.! bottomRight
  , clock2  = right  $ tiles V.! topRight
  , clock1  = top    $ tiles V.! topRight
  , clock11 = top    $ tiles V.! topLeft
  , clock10 = left   $ tiles V.! topLeft
  , clock8  = left   $ tiles V.! bottomLeft
  , clock7  = bottom $ tiles V.! bottomLeft
  , clock5  = bottom $ tiles V.! bottomRight
  }

validMacroTile :: TileStore -> MacroTile -> Bool
validMacroTile tiles MacroTile{..} =
  and [ ( top $ tiles V.! bottomRight ) == ( bottom $ tiles V.! topRight )
      , ( top $ tiles V.! bottomLeft )  == ( bottom $ tiles V.! topLeft )
      , ( right $ tiles V.! topLeft)    == ( left $ tiles V.! topRight )
      , ( right $ tiles V.! bottomLeft) == ( left $ tiles V.! bottomRight ) ]

computeMacroTiles :: TileStore -> V.Vector MacroTile
computeMacroTiles store = V.filter ( validMacroTile store ) $ do
  a <- indices
  b <- indices
  c <- indices
  d <- indices
  return $ MacroTile a b c d
  where numOfTiles = V.length store
        indices    = V.generate numOfTiles id


type LocalRelation = V.Vector MacroTile
type LocalRelations = M.Map MacroBoundary LocalRelation


-- here we created the values first as linked lists and then convert to vectors once we have looped through the TileStore
computeLocalRelations :: TileStore -> LocalRelations
computeLocalRelations store = V.fromList <$> V.foldl' action M.empty macroTiles
  where macroTiles = computeMacroTiles store
        action m tile =
          let boundary = macroBoundary store tile
          in case M.lookup boundary m of
            Nothing -> M.insert boundary [] m
            Just v -> M.insert boundary ( tile : v ) m


data Options = Options
  { seed :: ByteString
  , numberOfRows :: Int
  , numberOfColumns :: Int
  , rowCutOff :: Int
  , columnCutOff :: Int
  , tileWidth :: Int
  , tileHeight :: Int
  , vacuumTile :: Int
  , numberOfSteps :: Int
  } deriving (Show)

instance Csv.FromNamedRecord Options where
  parseNamedRecord m = Options <$>
    m .: "seed" <*>
    m .: "numberOfRows" <*>
    m .: "numberOfColumns" <*>
    m .: "rowCutOff" <*>
    m .: "columnCutOff" <*>
    m .: "tileWidth" <*>
    m .: "tileHeight" <*>
    m .: "vacuumTile" <*>
    m .: "numberOfSteps"

foreign import ccall "&freeTeaLeaf" freeTeaLeafC :: FunPtr (Ptr Word8 -> IO ())
foreign import ccall "generateTeaLeaf" generateTeaLeafC ::
  Int -> Int -> Int -> Int -> Int -> IO (Ptr Word8)


generateTeaLeafImage :: Options -> IO (JP.Image Word8)
generateTeaLeafImage Options{..} = do
  let size = numberOfRows * numberOfColumns
      PS hashPtr _ _ = ( hash seed )
  seedNum <- withForeignPtr ( castForeignPtr hashPtr ) peek
  ptr <- generateTeaLeafC seedNum
    numberOfRows numberOfColumns rowCutOff columnCutOff
  foreignPtr <- newForeignPtr freeTeaLeafC ptr
  vector <- S.freeze $ S.MVector size foreignPtr
  -- we set the random number generator here because seedNum is in scope
  -- this is not an ideal location, but we need a refactor to fix
  setStdGen $ mkStdGen seedNum
  return $ JP.Image numberOfColumns numberOfRows vector

data FusionContext = FusionContext
  { tileStore :: TileStore
  , localRelations :: LocalRelations
  , teaLeaf :: S.Vector Bool
  , fusionState :: SM.IOVector Int
  }

runFusion :: Options -> FusionContext -> IO ()
runFusion Options{..} FusionContext{..} = undefined

step :: Options -> FusionContext -> IO ()
step options@Options{..} FusionContext{..} = do
  let arrayLength = SM.length fusionState
  randomInt <- randomIO
  let index = randomInt `mod` arrayLength
  let (bottomRightIndex, topRightIndex, topLeftIndex, bottomLeftIndex) =
        macroTileIndices options $ indexToCoordinates options index

  return ()

indexToCoordinates :: Options  -> Int -> (Int,Int)
indexToCoordinates Options{..} index =
  (index `div` numberOfColumns, index `mod` numberOfColumns)

coordinatesToIndex :: Options -> (Int,Int) -> Int
coordinatesToIndex Options{..} (row,column) =
  row * numberOfColumns + column

{-
tile pattern for macro tiles

  2 1
  3 0
-}

macroTileIndices ::
  Options -> (Int,Int) -> (Int,Int,Int,Int)
macroTileIndices options@Options{..} (row,column) =
  ( coordinatesToIndex options (row' , column')
  , coordinatesToIndex options (row  , column')
  , coordinatesToIndex options (row  , column )
  , coordinatesToIndex options (row' , column )
  )
  where row'    = ( row + 1 ) `mod` numberOfRows
        column' = ( column + 1 ) `mod` numberOfColumns



word8ToBool :: Word8 -> Bool
word8ToBool w = if w == 0 then False else True

main :: IO ()
main = do
  args <- getArgs
  if Prelude.length (args) /= 1
    then Prelude.putStrLn "expecting single argument: tile directory"
    else do
      let folder = Prelude.head args
      setCurrentDirectory folder -- we work relative to a tile directory
      optionsByteString <- L.readFile "options.csv"
      let eitherOptions = Csv.decodeByName optionsByteString
      case eitherOptions of
        Left err -> putStrLn $ "error parsing options.csv: " <> err
        Right (_,optionsVector) -> do
          let options@Options{..} = V.head optionsVector
          teaLeafImage <- generateTeaLeafImage options
          JP.writePng ("tealeaf_" <> (unpack $ seed) <> ".png") teaLeafImage
          manifestByteString <- L.readFile "manifest.csv"
          let eitherTiles = Csv.decode Csv.NoHeader manifestByteString
          case eitherTiles of
            Left err -> putStrLn $ "error parsing manifest.csv: " <> err
            Right tileVector -> do
              let tileStore = tileVector
                  localRelations = computeLocalRelations tileStore
                  teaLeaf = S.map word8ToBool $ JP.imageData teaLeafImage
                  arrayLength = numberOfColumns * numberOfRows
              fusionState <- SM.new $ arrayLength
              SM.set fusionState vacuumTile
              let fusionContext = FusionContext
                    { tileStore = tileStore
                    , localRelations = localRelations
                    , teaLeaf = teaLeaf
                    , fusionState = fusionState
                    }
              return ()
