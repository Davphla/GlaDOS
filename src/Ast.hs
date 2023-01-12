module Ast (Ast) where

type Name = String

data Boolean = T | F

instance Show Boolean where
  show T = "#t"
  show F = "#f"

data ValueType
  = Integer Int
  | Boolean Boolean
  deriving (Show)

type Params = [String]

data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value ValueType             -- Value, either Boolean or Integer
  | Function Params Ast         -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args if needed
  deriving (Show)
