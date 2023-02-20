module Operator (Operator (..), OperatorType (..)) where

type Priority = Int
data OperatorType
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  deriving (Show, Eq, Read)

data Operator = Operator OperatorType Priority deriving (Eq, Show, Read)