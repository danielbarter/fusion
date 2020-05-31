{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Int (Int8)
import Foreign.Marshal.Alloc (free)
import Data.Primitive.PrimArray
import Data.Primitive.Ptr

foreign import ccall "generateTeaLeaf" generateTeaLeafC :: Int -> IO (Ptr Int8)

generateTeaLeaf :: IO (PrimArray Int8)
generateTeaLeaf = do
  ptr <- generateTeaLeafC 27
  teaLeafMutable <- newPrimArray (420 * 420)
  free ptr
  copyPtrToMutablePrimArray teaLeafMutable 0 ptr (420 * 420)
  unsafeFreezePrimArray teaLeafMutable

main :: IO ()
main = return ()
