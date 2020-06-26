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
import System.IO (openFile, IOMode(..),hPutStrLn, hClose)
import Control.Exception (Exception(..),throwIO)
import Control.Monad (replicateM_)


{-
boundary pattern for tiles
   1
  2 0
   3
this is the order of the boundary labels in manifest.csv

the last column in manifest.csv is the weight of the tile. The determines how often we want the tile to appear.
-}


data Tile = Tile
  { svg :: String
  , right :: Int
  , top :: Int
  , left :: Int
  , bottom :: Int
  , tileWeight :: Double
  } deriving (Show)

instance Csv.FromRecord Tile where
  parseRecord m =
    Tile <$> m .! 0 <*> m .! 1 <*>  m .! 2 <*> m .! 3 <*> m .! 4 <*> m .! 5



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
  , macroWeight :: Double
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
  return $ MacroTile a b c d $ foldr (*) 1 $ ( \i -> tileWeight $ store V.! i ) <$> [a,b,c,d]
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
            Nothing -> M.insert boundary [tile] m
            Just v -> M.insert boundary ( tile : v ) m


data ParsedOptions = ParsedOptions
  { parsedSeed :: ByteString
  , parsedNumberOfRows :: Int
  , parsedNumberOfColumns :: Int
  , parsedRowCutOff :: Int
  , parsedColumnCutOff :: Int
  , parsedTileWidth :: Int
  , parsedTileHeight :: Int
  , parsedVacuumTile :: Int
  , parsedNumberOfSteps :: Int
  } deriving (Show)

data Options = Options
  { seed :: ByteString
  , seedNum :: Int
  , numberOfRows :: Int
  , numberOfColumns :: Int
  , size :: Int
  , rowCutOff :: Int
  , columnCutOff :: Int
  , tileWidth :: Int
  , tileHeight :: Int
  , vacuumTile :: Int
  , numberOfSteps :: Int
  } deriving (Show)

generateOptions :: ParsedOptions -> IO Options
generateOptions ParsedOptions{..} = do
  let size = parsedNumberOfRows * parsedNumberOfColumns
      PS hashPtr _ _ = ( hash parsedSeed )
  seedNum <- withForeignPtr ( castForeignPtr hashPtr ) peek
  return $ Options
    { seed = parsedSeed
    , seedNum = seedNum
    , numberOfRows = parsedNumberOfRows
    , numberOfColumns = parsedNumberOfColumns
    , size = size
    , rowCutOff = parsedRowCutOff
    , columnCutOff = parsedColumnCutOff
    , tileWidth = parsedTileWidth
    , tileHeight = parsedTileHeight
    , vacuumTile = parsedVacuumTile
    , numberOfSteps = parsedNumberOfSteps
    }


instance Csv.FromNamedRecord ParsedOptions where
  parseNamedRecord m = ParsedOptions <$>
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
  (Ptr Double) -> Int -> Int -> Int -> Int -> IO (Ptr Word8)


generateTeaLeafImage :: Options -> (Ptr Double) -> IO (JP.Image Word8)
generateTeaLeafImage Options{..} ptrDouble  = do
  ptr <- generateTeaLeafC ptrDouble
    numberOfRows numberOfColumns rowCutOff columnCutOff
  foreignPtr <- newForeignPtr freeTeaLeafC ptr
  vector <- S.freeze $ S.MVector size foreignPtr
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
  randomInt <- randomIO
  let index = randomInt `mod` size
  if ( teaLeaf S.! index )
    then do
      let (bottomRightIndex, topRightIndex, topLeftIndex, bottomLeftIndex) =
            macroTileIndices options index
      bottomRightTile <- SM.read fusionState bottomRightIndex
      topRightTile    <- SM.read fusionState topRightIndex
      topLeftTile     <- SM.read fusionState topLeftIndex
      bottomLeftTile  <- SM.read fusionState bottomLeftIndex
      let macroTile = MacroTile
            { bottomRight = bottomRightTile
            , topRight = topRightTile
            , topLeft = topLeftTile
            , bottomLeft = bottomLeftTile
            , macroWeight = -1 -- we are only constructing this tile to compute it's boundary, so the weight is irrelevant
            }
          macroTileBoundary = macroBoundary tileStore macroTile
          mLocalRelation = M.lookup macroTileBoundary localRelations
      case mLocalRelation of
        Nothing -> throwIO $ MissingKey macroTileBoundary
        Just localRelation -> do
          let localRelationWeights = macroWeight <$> localRelation
              localRelationTotalWeight = sum localRelationWeights
              localRelationDistrubution = (\x -> x / localRelationTotalWeight) <$> localRelationWeights
          localRelationIndex <- sample localRelationDistrubution
          let newMacroTile = localRelation V.! localRelationIndex
          SM.write fusionState bottomRightIndex $ bottomRight newMacroTile
          SM.write fusionState topRightIndex    $ topRight newMacroTile
          SM.write fusionState topLeftIndex     $ topLeft newMacroTile
          SM.write fusionState bottomLeftIndex  $ bottomLeft newMacroTile
    else return ()

-- takes a distribution vector and produces a sample from it
sample :: V.Vector Double -> IO Int
sample v = do
  let l = V.length v
      cummulative = V.fromList $ ( \i -> sum $ V.take i v ) <$> [1..l]
  randomDouble <- randomIO :: IO Double
  let mindex = V.findIndex (\internal -> randomDouble < internal) cummulative
  case mindex of
    Nothing -> throwIO $ FailedSample randomDouble cummulative
    Just index -> return index

data FusionError =
  MissingKey MacroBoundary
  | FailedSample Double ( V.Vector Double )
  deriving (Show)

instance Exception FusionError

macroTileIndices ::
  Options -> Int -> (Int,Int,Int,Int)
macroTileIndices options@Options{..} index =
  let (row,column) = indexToCoordinates options index
      row'         = ( row + 1 ) `mod` numberOfRows
      column'      = ( column + 1 ) `mod` numberOfColumns
  in ( coordinatesToIndex (row' , column')
     , coordinatesToIndex (row  , column')
     , coordinatesToIndex (row  , column )
     , coordinatesToIndex (row' , column ))
  where
   coordinatesToIndex (r,c) = r * numberOfColumns + c

indexToCoordinates :: Options -> Int -> (Int,Int)
indexToCoordinates Options{..} i = (i `div` numberOfColumns, i `mod` numberOfColumns)



-- TODO: Currently this is very brittle. For example, it is white space sensitive. Want to actually parse the tile svg files
produceTiling :: Options -> FusionContext -> IO ()
produceTiling options@Options{..} FusionContext{..} = do
  frozenFusionState <- S.freeze fusionState
  let totalWidth    = numberOfColumns * tileWidth
      totalHeight   = numberOfRows * tileHeight
      svgBody index =
        let (row,column) = indexToCoordinates options index
            path = svg $ tileStore V.! ( frozenFusionState S.! index )
        in svgBodyHelper row column tileWidth tileHeight path
  handle <- openFile ( "tile_" <> (unpack $ seed) <> ".svg" )  WriteMode
  hPutStrLn handle $ svgHeader totalWidth totalHeight
  sequence_ $ ( \i -> do { tileBody <- svgBody i; if tileBody=="" then return () else hPutStrLn handle tileBody } ) <$> [0..(S.length frozenFusionState)-1]
  hPutStrLn handle svgTail
  hClose handle
  return ()
  where
    svgHeader width height = "<svg width="         <>
                             (show $ show width)   <>
                             " height="            <>
                             (show $ show height)  <>
                             " xmlns=\"http://www.w3.org/2000/svg\">"
    svgBodyHelper row col width height path = do
      bodyLines <- lines <$> readFile path
      if (length bodyLines == 2)
        then return ""
        else do
          let body = mconcat $ init $ tail bodyLines
          return $
            "<g transform=" <> (show $ "translate(" <> (show $ width * col) <> "," <> (show $ height * row) <> ")" ) <> ">" <> body <> "</g>"
    svgTail = "</svg>"


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
      let eitherParsedOptions = Csv.decodeByName optionsByteString
      case eitherParsedOptions of
        Left err -> putStrLn $ "error parsing options.csv: " <> err
        Right (_,parsedOptionsVector) -> do
          let parsedOptions = V.head parsedOptionsVector
          options@Options{..} <- generateOptions parsedOptions
          setStdGen $ mkStdGen seedNum
          let numberOfModes = 2 * ( rowCutOff * columnCutOff  + ( rowCutOff - 1 ) * ( columnCutOff - 1 ) - 1 )
          modeAmplitudesList <- sequence $ take numberOfModes $ repeat randomIO
          let modeAmplitudesVec = S.fromList $ 0:0:modeAmplitudesList
          SM.MVector _ modeForeignPtr <- S.thaw modeAmplitudesVec
          teaLeafImage <- withForeignPtr modeForeignPtr (generateTeaLeafImage options)
          JP.writePng ("tealeaf_" <> (unpack $ seed) <> ".png") teaLeafImage
          manifestByteString <- L.readFile "manifest.csv"
          let eitherTiles = Csv.decode Csv.NoHeader manifestByteString
          case eitherTiles of
            Left err -> putStrLn $ "error parsing manifest.csv: " <> err
            Right tileVector -> do
              let tileStore = tileVector
                  localRelations = computeLocalRelations tileStore
                  teaLeaf = S.map word8ToBool $ JP.imageData teaLeafImage
              fusionState <- SM.new $ size
              SM.set fusionState vacuumTile
              let fusionContext = FusionContext
                    { tileStore = tileStore
                    , localRelations = localRelations
                    , teaLeaf = teaLeaf
                    , fusionState = fusionState
                    }
              replicateM_ numberOfSteps $ step options fusionContext
              produceTiling options fusionContext
              return ()
