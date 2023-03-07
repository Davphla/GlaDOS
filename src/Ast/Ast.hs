{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast.hs
-}

module Ast.Ast (
  Ast (Define, Value, Lambda, Call, Operation, Condition),
  cptToAst,
  expressionToAst,
  operationToAst,
  listToParams,
  Params,
) where

import Ast.ShuntingYard (shuntingYard)
import Cpt.Cpt (
  Cpt (..),
  Assignement,
  getIdentifier,
  Condition,
  Expression,
  Operation,
  Lambda
  )
import Cpt.Literal (Literal (..))
import Cpt.Operator (Operator (..))
import Error (
  GlobalWarning (..),
  CptError (..),
  GladosError (..),
  CptErrorReason (InvalidCptNotTreatable)
  )


-- -------------------------------- Typedefs -------------------------------- --

type Name = String

type Params = [String]

data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value Literal               -- Value, either Bool, Int, Fraction or Float
  | Lambda Params Ast           -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args
  | Operation Operator [Ast]    -- Basic operators with its parameters
  | Condition Ast Ast Ast       -- Condition, then, else
  deriving (Eq, Show)


-- ------------------------ Ast generation functions ------------------------ --

listToParams :: [Cpt] -> Either [GladosError] Params
listToParams = mapM getIdentifier

splitOperatorParams :: Operator -> [Ast] -> Either [GladosError] [Ast]
splitOperatorParams op (p1:p2:rest) = Right $ Ast.Ast.Operation op [p2, p1] : rest
splitOperatorParams op _ = Left [Cpt $ InvalidCpt InvalidCptNotTreatable $
  "Invalid operation: " ++ show op ++ " missing parameters"]

operationToAst :: Operation -> Either [GladosError] [Ast]
operationToAst [] = Right []
operationToAst (Cpt.Cpt.Operator op:rest) = operationToAst rest >>=
  splitOperatorParams op
operationToAst (Cpt.Cpt.Literal l:rest) = operationToAst rest >>=
  (\next -> Right (Ast.Ast.Value l : next))
operationToAst (Cpt.Cpt.Identifier i:rest) = operationToAst rest >>=
  (\params -> Right [Ast.Ast.Call i (reverse params)])
operationToAst (Cpt.Cpt.Expression expr:_) = expressionToAst expr >>=
  Right . (:[])
operationToAst c = Left [Cpt $ InvalidCpt InvalidCptNotTreatable $ show c]

conditionToAst :: Condition -> Either [GladosError] Ast
conditionToAst (c, t, e) = cptToAst c >>= (\cond ->
  cptToAst t >>= (\then' ->
    cptToAst e >>= (Right . Ast.Ast.Condition cond then')))

lambdaToAst :: Lambda -> Either [GladosError] Ast
lambdaToAst _ = Left [Warning $ NotImplemented "lambdas"]
  -- listToParams ps >>= (\params ->
  -- cptToAst b >>= (Right . Ast.Ast.Lambda params))

expressionToAst :: Expression -> Either [GladosError] Ast
expressionToAst [Cpt.Cpt.Operation o] = shuntingYard o >>= (\x ->
  case (operationToAst . reverse) x of
  (Right [ast]) -> Right ast
  (Right _) -> Left [Cpt $ InvalidCpt InvalidCptNotTreatable $
    "invalid operation: " ++ show o]
  (Left err) -> Left err)
expressionToAst [Cpt.Cpt.Condition c] = conditionToAst c
expressionToAst [Cpt.Cpt.Lambda l] = lambdaToAst l
expressionToAst _ = Left [Warning $ NotImplemented "conditions"]

assignementToAst :: Assignement -> Either [GladosError] Ast
assignementToAst (s, l, Cpt.Cpt.Expression expr) = listToParams l >>=
  (\args -> expressionToAst expr >>= (Right . Define s . Ast.Ast.Lambda args))
assignementToAst c = Left [Cpt $ InvalidCpt InvalidCptNotTreatable $ show c]

cptToAst :: Cpt -> Either [GladosError] Ast
cptToAst (Cpt.Cpt.Prototype _) = Left [Warning $ NotImplemented "function prototypes"]
cptToAst (Cpt.Cpt.Assignement c) = assignementToAst c
cptToAst c = Left [Cpt $ InvalidCpt InvalidCptNotTreatable $ show c]
