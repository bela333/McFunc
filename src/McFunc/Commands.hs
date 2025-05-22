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

recMc :: String -> (DatapackM () -> DatapackM ()) -> DatapackM ()
recMc name f = join $ mfix $ \prev -> do
  datapack <- resolveDatapack prev
  let newDatapack =
        DatapackM $
          const
            DatapackRes
              { currentFunction = currentFunction datapack
              , files = empty
              , datapackResValue = ()
              }
  return $ asFunctionWithName name (f newDatapack)

say :: String -> DatapackM ()
say = runCommand . ("say " ++)
