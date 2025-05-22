module McFunc.Scoreboard where

import McFunc (CommandPart (CommandString), DatapackM, runRawCommand, unsafeNewFunction)
import McFunc.Selector

type Objective = String

data Scoreboard = Scoreboard Selector Objective

instance Show Scoreboard where
  show (Scoreboard selector objective) = show selector ++ " " ++ objective

data McBooleanCompare = McLT | McLE | McEQ | McGT | McGE

instance Show McBooleanCompare where
  show McLT = "<"
  show McLE = "<="
  show McEQ = "="
  show McGT = ">"
  show McGE = ">="

data McBoolean = McBoolean Scoreboard McBooleanCompare Scoreboard

instance Show McBoolean where
  show (McBoolean scoreboard1 compareSymbol scoreboard2) = show scoreboard1 ++ " " ++ show compareSymbol ++ " " ++ show scoreboard2

(<) :: Scoreboard -> Scoreboard -> McBoolean
(<) scoreboard1 = McBoolean scoreboard1 McLT
infix 4 <

(<=) :: Scoreboard -> Scoreboard -> McBoolean
(<=) scoreboard1 = McBoolean scoreboard1 McLE
infix 4 <=

(==) :: Scoreboard -> Scoreboard -> McBoolean
(==) scoreboard1 = McBoolean scoreboard1 McEQ
infix 4 ==

(>) :: Scoreboard -> Scoreboard -> McBoolean
(>) scoreboard1 = McBoolean scoreboard1 McGT
infix 4 >

(>=) :: Scoreboard -> Scoreboard -> McBoolean
(>=) scoreboard1 = McBoolean scoreboard1 McGE
infix 4 >=

ifThenElse :: McBoolean -> DatapackM () -> DatapackM () -> DatapackM ()
ifThenElse bool ifTrue ifFalse = do
  do
    ifTrueFunc <- unsafeNewFunction ifTrue
    runRawCommand [CommandString $ "execute if score " ++ show bool ++ " run function", ifTrueFunc]
  do
    ifFalseFunc <- unsafeNewFunction ifFalse
    runRawCommand [CommandString $ "execute unless score " ++ show bool ++ " run function", ifFalseFunc]
