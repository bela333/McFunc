module Main where

import McFunc

myFunction :: DatapackM ()
myFunction = do
  runCommand "say asd"
  runCommand "say asdasd"

myDatapack :: DatapackM ()
myDatapack = do
  _ <- newFunctionWithName "main.mcfunction" myFunction
  return ()

main :: IO ()
main = print $ files myDatapack
