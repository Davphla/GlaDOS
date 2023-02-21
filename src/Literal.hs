{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Literal.hs
-}

module Literal (
    Literal (..)
  ) where

data Literal
  = Int Int
  | Char Char
  | Float Double
  | Bool Bool
  | String [Char]
  | Array [Literal]
  | Expression
  deriving Eq

-- TODO : revoir la définition des types pour y ajouter les Listes et Tuples
-- TODO : réfléchir au format des enums (variables globales ? maintenant ?)

instance Show Literal where
  show (Int i) = show i
  show (Char c) = show c
  show (Float f) = show f
  show (Bool b) = if b then "#t" else "#f"
  show (String cs) = "\"" ++ cs ++ "\""
  show (Array ls) = show ls
  show Expression = ""
