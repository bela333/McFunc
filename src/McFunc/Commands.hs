module McFunc.Commands where

import Control.Monad (join)
import Control.Monad.Fix (mfix)
import Data.HashMap.Lazy (empty)
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

asFunctionWithName :: String -> DatapackM () -> DatapackM ()
asFunctionWithName name d = do
  newFunctionWithName name d
  ctx <- getContext
  runCommand $ "function " ++ datapackName ctx ++ ":" ++ name

rec :: String -> (DatapackM () -> DatapackM ()) -> DatapackM ()
rec name f = join $ mfix $ \prev -> return $ asFunctionWithName name (f $ loseFiles prev)

say :: String -> DatapackM ()
say = runCommand . ("say " ++)
