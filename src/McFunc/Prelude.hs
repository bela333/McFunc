{-# LANGUAGE NoImplicitPrelude #-}

module McFunc.Prelude (module Prelude, module McFunc, module McFunc.Commands, module McFunc.Selector, module McFunc.Scoreboard) where

import McFunc (DatapackM, writeFunction)
import McFunc.Commands
import McFunc.Scoreboard (McBoolean (McBoolean), McBooleanCompare (McLT), Scoreboard (..), ifThenElse, (<), (<=), (==), (>), (>=))
import McFunc.Selector (Selector (..), SelectorPart (..))
import Prelude hiding ((/=), (<), (<=), (==), (>), (>=))
