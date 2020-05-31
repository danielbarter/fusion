{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Vector.Storable
import Codec.Picture


foreign import ccall "&freeTeaLeaf" freeTeaLeafC :: FunPtr (Ptr Word8 -> IO ())
foreign import ccall "generateTeaLeaf" generateTeaLeafC ::
  Int -> Int -> Int -> Int -> Int -> IO (Ptr Word8)


generateTeaLeaf :: Int -> Int -> Int -> Int -> Int -> IO (Image Word8)
generateTeaLeaf seed numRows numCols rowCutoff columnCutoff = do
  let size = numRows * numCols
  ptr <- generateTeaLeafC seed numRows numCols rowCutoff columnCutoff
  foreignPtr <- newForeignPtr freeTeaLeafC ptr
  vector <- freeze $ MVector size foreignPtr
  return $ Image numCols numRows vector



main :: IO ()
main = generateTeaLeaf 34605832 1000 200 20 3 >>= ( writePng "tealeaf.png" )
