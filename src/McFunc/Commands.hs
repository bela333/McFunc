module McFunc.Commands where

import Data.HashMap.Strict (empty)
import McFunc

runCommand :: String -> DatapackM ()
runCommand cmd =
  DatapackM $
    const $
      DatapackRes
        { currentFunction = McFunction [cmd]
        , files = empty
        , datapackResValue = ()
        }

asFunction :: DatapackM () -> DatapackM ()
asFunction d = do
  path <- newFunction d
  ctx <- getContext
  runCommand $ "function " ++ datapackName ctx ++ ":" ++ path
