{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Word (Word8)
import Foreign.Marshal.Alloc (free)
import Data.Primitive.PrimArray
import Data.Primitive.Ptr

foreign import ccall "generateTeaLeaf" generateTeaLeafC ::
  Int -> Int -> Int -> Int -> IO (Ptr Word8)

generateTeaLeaf :: Int -> Int -> Int -> Int -> IO (PrimArray Word8)
generateTeaLeaf seed numRows numCols cutoff = do
  let size = numRows * numCols
  ptr <- generateTeaLeafC seed numRows numCols cutoff
  teaLeafMutable <- newPrimArray size
  free ptr
  copyPtrToMutablePrimArray teaLeafMutable 0 ptr size
  unsafeFreezePrimArray teaLeafMutable

main :: IO ()
main = generateTeaLeaf 42 420 420 5 >>= (putStrLn . show)
