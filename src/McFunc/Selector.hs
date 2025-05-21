module McFunc.Selector where

import Data.List (intercalate)

type Key = String
type Value = String

data SelectorPart
  = NameSelector String
  | TypeSelector String
  | OtherSelector Key Value

data Selector
  = Entity [SelectorPart]
  | Player [SelectorPart]
  | NearestPlayer [SelectorPart]
  | RandomPlayer [SelectorPart]
  | Executer [SelectorPart]
  | NearestEntity [SelectorPart]

showPart :: SelectorPart -> String
showPart (NameSelector value) = "name=" ++ value
showPart (TypeSelector value) = "type=" ++ value
showPart (OtherSelector key value) = key ++ "=" ++ value

showParts :: [SelectorPart] -> String
showParts [] = ""
showParts parts = "[" ++ intercalate "," (map showPart parts) ++ "]"

showSelector :: Selector -> String
showSelector (Entity parts) = "@e" ++ showParts parts
showSelector (Player parts) = "@p" ++ showParts parts
showSelector (NearestPlayer parts) = "@p" ++ showParts parts
showSelector (RandomPlayer parts) = "@r" ++ showParts parts
showSelector (Executer parts) = "@s" ++ showParts parts
showSelector (NearestEntity parts) = "@n" ++ showParts parts

instance Show Selector where
  show = showSelector
