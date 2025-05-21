{-# LANGUAGE NoImplicitPrelude #-}

module McFunc (module Prelude, module McFunc) where

import Control.Monad (ap, liftM)
import Data.HashMap.Strict
import Data.Hashable (Hashable (hashWithSalt), hash)
import Data.List (intercalate)
import Numeric (showHex)
import Prelude hiding ()

newtype McFunction = McFunction {runFunction :: [String]} deriving (Eq)

functionToFile :: McFunction -> String
functionToFile McFunction{runFunction = functionLines} = intercalate "\n" functionLines

instance Hashable McFunction where
  hashWithSalt salt (McFunction runFunction) = hashWithSalt salt runFunction

instance Show McFunction where
  show :: McFunction -> String
  show = show . functionToFile

data DatapackM a = DatapackM
  { currentFunction :: McFunction
  , files :: HashMap FilePath McFunction
  , runDatapackM :: a
  }

newFunctionWithName :: String -> DatapackM () -> DatapackM String
newFunctionWithName path DatapackM{currentFunction, files} =
  DatapackM
    { currentFunction = McFunction []
    , files = insert (path ++ ".mcfunction") currentFunction files
    , runDatapackM = path
    }

formatHash :: Int -> String
formatHash n
  | 0 <= n = showHex n ""
  | otherwise = 'n' : showHex (-n) ""

newFunction :: DatapackM () -> DatapackM String
newFunction datapack = newFunctionWithName name datapack
 where
  name = formatHash (hash $ currentFunction datapack)

instance Monad DatapackM where
  return = pure
  a >>= b =
    DatapackM
      { files = files a `union` files bRes
      , currentFunction = McFunction (runFunction (currentFunction a) ++ runFunction (currentFunction bRes))
      , runDatapackM = runDatapackM bRes
      }
   where
    bRes = b (runDatapackM a)

instance Applicative DatapackM where
  pure x = DatapackM{currentFunction = McFunction [], files = empty, runDatapackM = x}
  (<*>) = ap

instance Functor DatapackM where
  fmap = liftM

-- User facing function

runCommand :: String -> DatapackM ()
runCommand cmd = DatapackM{currentFunction = McFunction [cmd], files = empty, runDatapackM = ()}

runAsFunction :: DatapackM () -> DatapackM ()
runAsFunction d = do
  path <- newFunction d
  runCommand $ "function datapack:" ++ path
