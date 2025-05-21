{-# LANGUAGE RebindableSyntax #-}

module Main where

import McFunc.Prelude

myFunction :: DatapackM ()
myFunction = do
  say "asd"
  say "asdasd"
  asFunction $ do
    say "test"
  let bela = NearestPlayer [NameSelector "bela333"]
  let belaFoo = Scoreboard bela "foo"
  let belaBar = Scoreboard bela "bar"
  if belaFoo > belaBar
    then do
      say "More Foo"
      say "Nice"
    else do
      say "More Bar"
      say "Cool"

myDatapack :: DatapackM ()
myDatapack = do
  newFunctionWithName "main" myFunction
  return ()

main :: IO ()
main = writeDatapack myDatapack "mydatapack" "datapack"
