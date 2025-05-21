module McFunc (module McFunc) where

import Control.Monad (ap, forM_, liftM)
import Data.HashMap.Strict
import Data.Hashable (Hashable (hashWithSalt), hash)
import Data.List (intercalate)
import Numeric (showHex)
import System.FilePath ((</>))

type DatapackName = String

newtype McFunction = McFunction {runFunction :: [String]} deriving (Eq)

functionToFile :: McFunction -> String
functionToFile McFunction{runFunction = functionLines} = intercalate "\n" functionLines

instance Show McFunction where
  show :: McFunction -> String
  show = show . functionToFile

instance Hashable McFunction where
  hashWithSalt salt (McFunction runFunction) = hashWithSalt salt runFunction

newtype DatapackContext = DatapackContext {datapackName :: DatapackName}

data DatapackRes a = DatapackRes
  { currentFunction :: McFunction
  , files :: HashMap FilePath McFunction
  , datapackResValue :: a
  }

newtype DatapackM a = DatapackM {runDatapackM :: DatapackContext -> DatapackRes a}

instance Monad DatapackM where
  return = pure
  a >>= b = DatapackM $ \ctx ->
    let aRes = runDatapackM a ctx
     in let bRes = runDatapackM (b (datapackResValue aRes)) ctx
         in DatapackRes
              { currentFunction = McFunction $ runFunction (currentFunction aRes) ++ runFunction (currentFunction bRes)
              , files = files aRes `union` files bRes
              , datapackResValue = datapackResValue bRes
              }

instance Applicative DatapackM where
  pure x =
    DatapackM $
      const
        DatapackRes
          { currentFunction = McFunction []
          , files = empty
          , datapackResValue = x
          }
  (<*>) = ap

instance Functor DatapackM where
  fmap = liftM

getContext :: DatapackM DatapackContext
getContext = DatapackM $ \ctx ->
  DatapackRes
    { currentFunction = McFunction []
    , files = empty
    , datapackResValue = ctx
    }

resolveDatapack :: DatapackM a -> DatapackM (DatapackRes a)
resolveDatapack datapack = runDatapackM datapack <$> getContext

newFunctionWithName :: String -> DatapackM () -> DatapackM ()
newFunctionWithName name datapack = do
  DatapackRes{currentFunction, files} <- resolveDatapack datapack
  DatapackM $
    const $
      DatapackRes
        { currentFunction = McFunction []
        , files = insert (name ++ ".mcfunction") currentFunction files
        , datapackResValue = ()
        }

formatHash :: Int -> String
formatHash n
  | 0 <= n = showHex n ""
  | otherwise = 'n' : showHex (-n) ""

newFunction :: DatapackM () -> DatapackM String
newFunction datapack = do
  DatapackRes{currentFunction} <- resolveDatapack datapack
  let name = formatHash (hash currentFunction)
  newFunctionWithName name datapack
  return name

-- User facing functions

writeDatapack :: DatapackM () -> DatapackName -> FilePath -> IO ()
writeDatapack datapack name path = do
  let DatapackRes{files} = runDatapackM datapack DatapackContext{datapackName = name}
  let fileEntries = toList files
  forM_ fileEntries $ \(filename, function) -> do
    let functionPath = path </> filename
    let functionContent = functionToFile function
    writeFile functionPath functionContent
