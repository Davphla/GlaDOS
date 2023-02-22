module Operator (Operator (..), OperatorType (..), operatorFromStr) where

type Priority = Int
data OperatorType
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  deriving (Show, Eq, Read)

data Operator = Operator OperatorType Priority deriving (Eq, Show, Read)

operatorFromStr :: String -> Maybe Operator
operatorFromStr "+" = Just $ Operator Plus 10
operatorFromStr "-" = Just $ Operator Minus 10
operatorFromStr "*" = Just $ Operator Times 20
operatorFromStr "/" = Just $ Operator Div 20
operatorFromStr "%" = Just $ Operator Mod 20
operatorFromStr _ = Nothing