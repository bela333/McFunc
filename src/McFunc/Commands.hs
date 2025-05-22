module McFunc.Commands where

import McFunc

runCommand :: String -> DatapackM ()
runCommand cmd =
  DatapackM $
    const $
      DatapackRes
        { currentFunction = [[CommandString cmd]]
        , files = []
        , datapackResValue = ()
        }

say :: String -> DatapackM ()
say = runCommand . ("say " ++)
