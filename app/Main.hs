{-# LANGUAGE RebindableSyntax #-}

module Main where

import Control.Monad (forM_)
import Data.HashMap.Strict (toList)
import McFunc
import System.FilePath

myFunction :: DatapackM ()
myFunction = do
  runCommand "say asd"
  runCommand "say asdasd"
  runAsFunction $ do
    runCommand "say test"

myDatapack :: DatapackM ()
myDatapack = do
  _ <- newFunctionWithName "main" myFunction
  return ()

main :: IO ()
main = do
  let datapackFiles = toList $ files myDatapack
  let datapackPath :: FilePath
      datapackPath = "datapack"
  forM_ datapackFiles $ \(functionName, function) -> do
    let filePath = datapackPath </> functionName
    writeFile filePath $ functionToFile function
    return ()
