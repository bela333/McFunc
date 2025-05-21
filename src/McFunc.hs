module McFunc where

import Control.Monad (ap, liftM)
import Data.HashMap.Strict
import Data.Hashable (Hashable (hashWithSalt), hash)
import Data.List (intercalate)

newtype McFunction = McFunction {runFunction :: [String]} deriving (Eq)

instance Hashable McFunction where
  hashWithSalt salt (McFunction runFunction) = hashWithSalt salt runFunction

instance Show McFunction where
  show :: McFunction -> String
  show (McFunction functionLines) = show $ intercalate "\n" functionLines

data DatapackM a = DatapackM
  { currentFunction :: McFunction
  , files :: HashMap String McFunction
  , runDatapackM :: a
  }

runCommand :: String -> DatapackM ()
runCommand cmd = DatapackM{currentFunction = McFunction [cmd], files = empty, runDatapackM = ()}

newFunctionWithName :: String -> DatapackM () -> DatapackM String
newFunctionWithName name DatapackM{currentFunction, files} =
  DatapackM
    { currentFunction = McFunction []
    , files = insert name currentFunction files
    , runDatapackM = name
    }

newFunction :: DatapackM () -> DatapackM String
newFunction datapack = newFunctionWithName name datapack
 where
  name = show (hash $ currentFunction datapack) ++ ".mcfunction"

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
