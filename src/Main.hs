{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.Ptr

foreign import ccall "generateTeaLeaf" generateTeaLeaf :: Int -> IO (Ptr Bool)

main :: IO ()
main = do
  ptr <- generateTeaLeaf 0
  return ()
