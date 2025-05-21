{-# LANGUAGE RebindableSyntax #-}

module Main where

import McFunc.Prelude

myFunction :: DatapackM ()
myFunction = do
  runCommand "say asd"
  runCommand "say asdasd"
  asFunction $ do
    runCommand "say test"

myDatapack :: DatapackM ()
myDatapack = do
  _ <- newFunctionWithName "main" myFunction
  return ()

main :: IO ()
main = writeDatapack myDatapack "test" "datapack"
