{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Cpt.hs
-}

module Cpt.Cpt (
    Cpt (..),
    Operation,
    getIdentifier, getKeyword, getLiteral, getList, getOperator
  ) where

import Cpt.Literal (Literal)
import Cpt.Operator (Operator)
import Cpt.Keyword (Keyword)
import Error (GladosError (Cpt), CptError (InvalidCpt))

type Identifier = String
type Expression = [Cpt]
type Condition = (Cpt, Cpt, Cpt)
type Operation = [Cpt]
type Assignement = (String, [Cpt], Cpt)
type Prototype = (String, [Identifier])
type Lambda = [Cpt]

data Cpt
  = Literal Literal
  | Identifier String
  | Keyword Keyword
  | Operator Operator
  | Expression Expression
  | Condition Condition
  | Operation Operation
  | Assignement Assignement
  | Prototype Prototype
  | Lambda Lambda
  deriving (Eq)


instance Show Cpt where
  show (Literal l) = show l
  show (Identifier s) = s
  show (Keyword k) = show k
  show (Operator o) = show o
  show (Expression (l:ls)) = "Expression " ++ foldl (\x acc -> x ++ " " ++ acc) (show l) (map show ls)
  show (Expression []) = "empty Cpt"
  show (Condition (a, b, c)) = "if " ++ show a ++ " then " ++ show b ++ " else " ++ show c
  show (Operation (l:ls)) = "Operation " ++ foldl (\x acc -> x ++ " " ++ acc) (show l) (map show ls)
  show (Operation []) = "empty Cpt"
  show (Assignement (s, l, c)) = s ++ " " ++ show l ++ " = " ++ show c
  show (Prototype (s, l)) = s ++ " " ++ show l
  show (Lambda l) = "lambda " ++ show l

getIdentifier :: Cpt -> Either [GladosError] String
getIdentifier (Identifier s) = Right s
getIdentifier _ = Left [Cpt InvalidCpt]

getLiteral :: Cpt -> Either [GladosError] Literal
getLiteral (Literal x) = Right x
getLiteral _ = Left [Cpt InvalidCpt]

getKeyword :: Cpt -> Either [GladosError] Keyword
getKeyword (Keyword k) = Right k
getKeyword _ = Left [Cpt InvalidCpt]

getOperator :: Cpt -> Either [GladosError] Operator
getOperator (Operator o) = Right o
getOperator _ = Left [Cpt InvalidCpt]

getList :: Cpt -> Either [GladosError] [Cpt]
getList (Expression l) = Right l
getList _ = Left [Cpt InvalidCpt]

-- Faire une fonction qui crée l'abre correspondant à une expression en
-- ajoutant les priorités.

-- Les évaluations sont faites au moment de la création de l'Ast, si
-- l'expression est évaluable, sinon elle est simplement stockée dans
-- la table des symboles.