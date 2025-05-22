{-# LANGUAGE RebindableSyntax #-}

module Main where

import McFunc.Prelude

myFunction :: DatapackM ()
myFunction = do
  say "asd"
  say "asdasd"
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
  if belaFoo <= belaBar
    then do
      say "Less Foo"
    else do
      say "Less Bar"

main :: IO ()
main = do
  putStrLn "Generating datapack..."
  writeFunction myFunction "main" "mydatapack" "datapack"
