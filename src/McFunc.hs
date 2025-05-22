module McFunc (module McFunc) where

import Control.Monad (ap, forM_, liftM)
import Control.Monad.Fix (MonadFix (mfix))
import Data.Hashable (Hashable (hashWithSalt), hash)
import Data.List (intercalate, nub)
import Numeric (showHex)
import System.FilePath ((</>))

type DatapackName = String

data CommandPart = CommandString String | CommandFunc Int deriving (Eq)
type Command = [CommandPart]
type McFunction = [Command]

instance Show CommandPart where
  show (CommandString str) = str
  show (CommandFunc ref) = "\\" ++ show ref ++ "\\"

instance Hashable CommandPart where
  hashWithSalt salt (CommandString str) = salt `hashWithSalt` (0 :: Int) `hashWithSalt` str
  hashWithSalt salt (CommandFunc n) = salt `hashWithSalt` (1 :: Int) `hashWithSalt` n

refPatchPart :: Int -> CommandPart -> CommandPart
refPatchPart n (CommandFunc m) = CommandFunc $ n + m
refPatchPart _ a = a

showCommand :: Command -> String
showCommand command = unwords $ map show command

refPatchCommand :: Int -> Command -> Command
refPatchCommand n = map (refPatchPart n)

showFunction :: McFunction -> String
showFunction function = intercalate "\n" $ map showCommand function

refPatchFunction :: Int -> McFunction -> McFunction
refPatchFunction n = map (refPatchCommand n)

newtype DatapackContext = DatapackContext {datapackName :: DatapackName}

data DatapackRes a = DatapackRes
  { currentFunction :: McFunction
  , files :: [McFunction]
  , datapackResValue :: a
  }

newtype DatapackM a = DatapackM {runDatapackM :: DatapackContext -> DatapackRes a}

instance Monad DatapackM where
  return = pure
  a >>= b = DatapackM $ \ctx ->
    let aRes = runDatapackM a ctx
     in let bRes = runDatapackM (b (datapackResValue aRes)) ctx
         in let fileLen = length $ files aRes
             in DatapackRes
                  { currentFunction = currentFunction aRes ++ refPatchFunction fileLen (currentFunction bRes)
                  , files = files aRes ++ map (refPatchFunction fileLen) (files bRes)
                  , datapackResValue = datapackResValue bRes
                  }

instance Applicative DatapackM where
  pure x =
    DatapackM $
      const
        DatapackRes
          { currentFunction = []
          , files = []
          , datapackResValue = x
          }
  (<*>) = ap

instance Functor DatapackM where
  fmap = liftM

instance MonadFix DatapackM where
  mfix f = DatapackM $ \ctx -> let go = runDatapackM (f (datapackResValue go)) ctx in go

runRawCommand :: Command -> DatapackM ()
runRawCommand command =
  DatapackM $
    const
      DatapackRes
        { currentFunction = [command]
        , files = []
        , datapackResValue = ()
        }

getContext :: DatapackM DatapackContext
getContext = DatapackM $ \ctx ->
  DatapackRes
    { currentFunction = []
    , files = []
    , datapackResValue = ctx
    }

resolveDatapack :: DatapackM a -> DatapackM (DatapackRes a)
resolveDatapack datapack = runDatapackM datapack <$> getContext

formatHash :: Int -> String
formatHash n
  | 0 <= n = showHex n ""
  | otherwise = 'n' : showHex (-n) ""

-- Adds function to monad without calling `function` on it
-- Returned CommandPart must be used before any new files are created
unsafeNewFunction :: DatapackM () -> DatapackM CommandPart
unsafeNewFunction datapack = DatapackM $ \ctx ->
  let DatapackRes{files, currentFunction} = runDatapackM datapack ctx
   in DatapackRes
        { currentFunction =
            []
        , files = files ++ [currentFunction]
        , -- Return value refers to the previous file in the stack (`currentFunction`)
          datapackResValue = CommandFunc $ -1
        }

asFunction :: DatapackM () -> DatapackM ()
asFunction datapack = do
  part <- unsafeNewFunction datapack
  runRawCommand [CommandString "function", part]

rec :: (DatapackM () -> DatapackM ()) -> DatapackM ()
rec f = do
  asFunction $
    f $
      DatapackM $
        const
          DatapackRes
            { files = []
            , currentFunction = [[CommandString "function", CommandFunc 0]]
            , datapackResValue = ()
            }

-- User facing functions

writeFunction :: DatapackM () -> String -> DatapackName -> FilePath -> IO ()
writeFunction datapack functionName name path = do
  let DatapackRes{files, currentFunction} = runDatapackM datapack DatapackContext{datapackName = name}
  let substCommandPart :: CommandPart -> String
      substCommandPart (CommandString str) = str
      substCommandPart (CommandFunc n) = name ++ ":" ++ formatHash (hash $ files !! n)
  let encodeFunction :: McFunction -> String
      encodeFunction = intercalate "\n" . map (unwords . map substCommandPart)
  -- `nub` could be replaced with a faster alternative
  forM_ (nub files) $ \file -> do
    let filepath = path </> formatHash (hash file) ++ ".mcfunction"
    let encodedFile = encodeFunction file
    writeFile filepath encodedFile
  let mainpath = path </> functionName ++ ".mcfunction"
  let encodedMain = encodeFunction currentFunction
  writeFile mainpath encodedMain
