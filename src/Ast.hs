{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast.hs
-}

module Ast (
  Ast (Define, Value, Lambda, Call, Operator),
  cptToAst,
  listToParams,
  expressionToAst,
  shuntingYard,
  Params,
) where

import Cpt.Cpt (Cpt (..), getIdentifier)
import Cpt.Keyword (Keyword (..))
import Cpt.Literal (Literal (..))
import Cpt.Operator (Operator (..))
import Error (AstError (..), GladosError (..))

import GHC.Generics (Associativity (..))

type Name = String

type Params = [String]


data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value Literal               -- Value, either Bool, Int, Fraction or Float
  | Lambda Params Ast           -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args if needed
  | Operator Operator [Ast]     -- Basic operators with its parameters
  deriving (Eq, Show)


listToParams :: [Cpt] -> Either [GladosError] Params
listToParams = mapM getIdentifier

listToArgs :: [Cpt] -> Either [GladosError] [Ast]
listToArgs = mapM cptToAst

symbolToAst :: String -> Either [GladosError] Ast
symbolToAst s = Right (Call s [])

keywordToAst :: Keyword -> Either [GladosError] Ast
keywordToAst _ = Left [Ast InvalidAst]

-- defineToAst :: [Cpt] -> Maybe Ast
-- defineToAst [Identifier a, b] = cptToAst b >>= (Just . Define a)
-- defineToAst [List (Identifier n:ps), b] = listToParams ps >>=
--     (\params -> cptToAst b >>= (Just . Function params)) >>= (Just . Define n)
-- defineToAst _ = Nothing

expressionToAst :: [Cpt] -> Either [GladosError] Ast
expressionToAst [Keyword Cpt.Keyword.Lambda, Cpt.Cpt.Expression ps, b] = listToParams ps >>= (\params ->
  cptToAst b >>= (Right . Ast.Lambda params))
expressionToAst (Identifier s:ps) = listToArgs ps >>= (Right . Call s)
expressionToAst _ = Left [Ast NotImplemented]

operationToAst :: [Cpt] -> Either [GladosError] Ast
operationToAst _ = Left [Ast NotImplemented]

operatorToAst :: Operator -> Either [GladosError] Ast
operatorToAst _ = Left [Ast InvalidAst]

precedence :: Cpt -> Int
precedence (Cpt.Cpt.Operator (Cpt.Operator.Operator _ p _)) = p
precedence _ = 0

associativity :: Cpt -> Associativity
associativity (Cpt.Cpt.Operator (Cpt.Operator.Operator _ _ a)) = a
associativity _ = LeftAssociative

-- ioShuntingYard' :: [Cpt] -> [Cpt] -> [Cpt] -> IO (Either [GladosError] [Cpt])
-- ioShuntingYard' out ops [] = return $ Right $ out ++ ops -- End of input
-- ioShuntingYard' out ops ((Cpt.Cpt.Literal v):ts) = ioShuntingYard' (out ++ [Cpt.Cpt.Literal v]) ops ts -- Literal
-- ioShuntingYard' out ops ((Cpt.Cpt.Operation ls):ts) = print ops >> ioShuntingYard' [] [] ls -- Parenthesis
--   >>= (\(Right xs) -> ioShuntingYard' (out ++ xs) ops ts)
-- ioShuntingYard' out ops (op:ts) = print ops >> ioShuntingYard' newOut newOps ts where -- Operator and Identifier handling
--   (topOps, restOps) = span (\c -> precedence c > precedence op ||
--     (precedence c == precedence op && associativity c == LeftAssociative)) ops
--   newOut = out ++ topOps
--   newOps = op : restOps

shuntingYard' :: [Cpt] -> [Cpt] -> [Cpt] -> Either [GladosError] [Cpt]
shuntingYard' out ops [] = Right $ out ++ ops -- End of input
shuntingYard' out ops ((Cpt.Cpt.Literal v):ts) = shuntingYard' (out ++ [Cpt.Cpt.Literal v]) ops ts -- Literal
shuntingYard' out ops ((Cpt.Cpt.Operation ls):ts) = shuntingYard' [] [] ls -- Parenthesis
  >>= (\xs -> shuntingYard' (out ++ xs) ops ts)
shuntingYard' out ops (op:ts) = shuntingYard' newOut newOps ts where -- Operator and Identifier handling
  (topOps, restOps) = span (\c -> precedence c > precedence op ||
    (precedence c == precedence op && associativity c == LeftAssociative)) ops
  newOut = out ++ topOps
  newOps = op : restOps

shuntingYard :: [Cpt] -> Either [GladosError] [Cpt]
shuntingYard = shuntingYard' [] []

cptToAst :: Cpt -> Either [GladosError] Ast
cptToAst (Cpt.Cpt.Literal i) = Right (Value i)
cptToAst (Cpt.Cpt.Operation o) = shuntingYard o >>= operationToAst
cptToAst (Cpt.Cpt.Identifier s) = symbolToAst s
cptToAst (Cpt.Cpt.Expression l) = expressionToAst l
cptToAst (Cpt.Cpt.Keyword k) = keywordToAst k
cptToAst (Cpt.Cpt.Operator o) = operatorToAst o
cptToAst (Cpt.Cpt.Prototype _) = Left [Ast NotImplemented]
cptToAst (Cpt.Cpt.Assignement _) = Left [Ast NotImplemented]
cptToAst (Cpt.Cpt.Condition _) = Left [Ast NotImplemented]
cptToAst (Cpt.Cpt.Lambda _) = Left [Ast NotImplemented]
