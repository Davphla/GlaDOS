{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Literal.hs
-}

module Literal (
    Literal (Integer, Inexact, Floating, Boolean)
  ) where

data Literal
  = Integer Int
  | Inexact Int Int
  | Floating Double
  | Boolean Bool
  deriving Eq

instance Show Literal where
  show (Integer i) = show i
  show (Inexact n d) = show n ++ "/" ++ show d
  show (Floating f) = show f
  show (Boolean b) = if b then "#t" else "#f"