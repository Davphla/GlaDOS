{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Operator.hs
-}

module Cpt.Operator (Operator (..), OperatorType (..), operatorFromStr, operators) where

type Priority = Int
data OperatorType
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  deriving (Show, Eq, Read)

data Operator = Operator OperatorType Priority deriving (Eq, Show, Read)

operators :: [String]
operators = [".", "+", "-", "*", "/", "`function`", "::", "->", "=", "$"]

operatorFromStr :: String -> Maybe Operator
operatorFromStr "+" = Just $ Operator Plus 10
operatorFromStr "-" = Just $ Operator Minus 10
operatorFromStr "*" = Just $ Operator Times 20
operatorFromStr "/" = Just $ Operator Div 20
operatorFromStr "%" = Just $ Operator Mod 20
operatorFromStr _ = Nothing
