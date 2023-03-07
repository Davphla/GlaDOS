{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Operator.hs
-}

module Cpt.Operator (Operator (..), OperatorType (..), operatorFromStr, operators) where

import GHC.Generics (Associativity (..))

type Priority = Int
data OperatorType
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  deriving (Show, Eq, Read)

data Operator = Operator OperatorType Priority Associativity deriving (Eq, Read)

instance Show Operator where
  show (Operator Plus _ _) = "+"
  show (Operator Minus _ _) = "-"
  show (Operator Times _ _) = "*"
  show (Operator Div _ _) = "/"
  show (Operator Mod _ _) = "%"

operators :: [String]
operators = [".", "+", "-", "*", "/", "`function`", "::", "->", "=", "$"]

operatorFromStr :: String -> Maybe Operator
operatorFromStr "+" = Just $ Operator Plus 10 LeftAssociative
operatorFromStr "-" = Just $ Operator Minus 10 LeftAssociative
operatorFromStr "*" = Just $ Operator Times 20 LeftAssociative
operatorFromStr "/" = Just $ Operator Div 20 LeftAssociative
operatorFromStr "%" = Just $ Operator Mod 20 LeftAssociative
operatorFromStr _ = Nothing
