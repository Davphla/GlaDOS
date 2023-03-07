{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Operator.hs
-}

module Operator (Operator (..), OperatorType (..)) where

import GHC.Generics (Associativity)

type Precedence = Int
data OperatorType
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  deriving (Eq)

instance Show OperatorType where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
  show Mod = "%"

data Operator = Operator OperatorType Precedence Associativity deriving (Eq)

instance Show Operator where
  show (Operator o _ _) = show o