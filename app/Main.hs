{-# LANGUAGE RebindableSyntax #-}

module Main where

import McFunc.Prelude
import McFunc.Scoreboard (Scoreboard (Scoreboard))

myFunction :: DatapackM ()
myFunction = do
  runCommand "say asd"
  runCommand "say asdasd"
  asFunction $ do
    runCommand "say test"
  let bela = NearestPlayer [NameSelector "bela333"]
  let belaFoo = Scoreboard bela "foo"
  let belaBar = Scoreboard bela "bar"
  let moreFoo = belaFoo > belaBar
  if moreFoo
    then do
      say "More Foo"
      say "Nice"
    else do
      say "More Bar"
      say "Cool"

  return ()

myDatapack :: DatapackM ()
myDatapack = do
  newFunctionWithName "main" myFunction
  return ()

main :: IO ()
main = writeDatapack myDatapack "test" "datapack"
