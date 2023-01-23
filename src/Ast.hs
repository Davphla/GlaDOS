module Ast (Ast, cptToAst) where

-- import Binding (Binding)
import Cpt (Cpt (Integer, Symbol, List))
import Data.Maybe (fromJust)

type Name = String

data Boolean = T | F

instance Show Boolean where
  show T = "#t"
  show F = "#f"

data ValueType
  = Number Int
  | Boolean Bool
  deriving (Show)

type Params = [String]

data Operator = Plus | Minus | Times | Div | Mod
    deriving Show

data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value ValueType             -- Value, either Boolean or Integer
  | Function Params Ast         -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args if needed
  | Operator Operator [Ast]     -- Basic operators with its parameters
  deriving (Show)


cptToAst :: Cpt -> Maybe Ast
cptToAst (Integer i) = Just (Value (Number i))
cptToAst (Symbol "#t") = Just (Value (Boolean True))
cptToAst (Symbol "#f") = Just (Value (Boolean False))
cptToAst (Symbol s) = Just (Call s [])
cptToAst (List [Symbol "define", Symbol a, b]) = Just (Define a (fromJust (cptToAst b)))
cptToAst (List (Symbol s:params)) = case mapM cptToAst params of
    Just as -> Just (Call s as)
    _ -> Nothing
cptToAst _ = Nothing


-- call :: Name -> [Ast] -> Maybe Ast
-- call n as = case getBinding n of
--     (Function ps b) -> evalAST (getVariables b ps as)
--     (Value x) -> Just x
--     _ -> Nothing


getOperator :: (Integral a) => Operator -> (a -> a -> a)
getOperator Plus = (+)
getOperator Minus = (-)
getOperator Times = (*)
getOperator Div = div
getOperator Mod = mod

evalAST :: Ast -> Maybe Ast
evalAST (Value x) = Just (Value x)
evalAST (Define n b) = Nothing -- TODO add the variable to the mappings
evalAST (Function p b) = Nothing -- TODO execute the function by creating a new environment
evalAST (Call n as) = Nothing -- TODO get the corresponding variable
evalAST (Operator op (p:ps)) = Just (Value (Number $ foldr (getOperator op) 0 [1, 2, 3, 4])) -- Normally p and ps are the args, but we still need to convert them to Values.
evalAST (Operator _ []) = Just (Value (Number 0))
