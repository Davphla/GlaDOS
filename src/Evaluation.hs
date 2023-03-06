{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Evaluation.hs
-}

module Evaluation (evalAst, Bindings) where

import Ast (
    Ast (Define, Lambda, Value, Call, Operator)
  )
import Cpt.Literal (Literal (Int))
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import Cpt.Operator


-- -------------------------------- Bindings -------------------------------- --

type Bindings = Map String Ast

-- ---------------------------- Builtin operators --------------------------- --

data BuiltinOperator a = BuiltinOperator {
  function :: a -> a -> a
  , minArgs :: Int
  , neutral :: a
}

-- ------------------------------- Evaluation ------------------------------- --

-- getOperator :: Integral a => Operator -> BuiltinOperator a
-- getOperator (Operator.Operator Plus _) = BuiltinOperator {function = (+), minArgs = 0, neutral = 0}
-- getOperator (Operator.Operator Minus _) = BuiltinOperator {function = (-), minArgs = 1, neutral = 0}
-- getOperator (Operator.Operator Times _) = BuiltinOperator {function = (*), minArgs = 0, neutral = 1}
-- getOperator (Operator.Operator Div _) = BuiltinOperator {function = div, minArgs = 1, neutral = 1}
-- getOperator (Operator.Operator Mod _) = BuiltinOperator {function = mod, minArgs = 1, neutral= 1}

getNeutral :: Integral a => BuiltinOperator a -> [Ast] -> Bindings -> Maybe a
getNeutral op as bs
  | length as < minArgs op = Nothing
  | length as == minArgs op = Just $ neutral op
  | otherwise = fst (evalAst (head as) bs) >>= extractValue

removeNeutral :: BuiltinOperator a -> [Ast] -> [Ast]
removeNeutral op as
  | length as <= minArgs op = as
  | otherwise = tail as

processOperator :: Integral a => BuiltinOperator a -> [Ast] -> Bindings -> Maybe a
processOperator op as bs
  | length as < minArgs op = Nothing
  | otherwise = getNeutral op as bs >>= \n -> Just (foldl (function op) n (fromJust (
    mapM (extractValue . fromJust . fst . (`evalAst` bs)) (removeNeutral op as))))

extractValue :: Integral a => Ast -> Maybe a
extractValue (Value (Int i)) = Just $ fromIntegral i
extractValue _ = Nothing

functionBindings :: Bindings -> [Ast] -> [String] -> Maybe Bindings
functionBindings bs [] [] = Just bs
functionBindings bs (a:as) (n:ns) = functionBindings (insert n a bs) as ns
functionBindings _ _ _ = Nothing

processLambda :: Bindings -> [Ast] -> [String] -> Ast -> Maybe Ast
processLambda bs as ns body = functionBindings bs as ns >>= fst . evalAst body

processCall :: Bindings -> String -> [Ast] -> Maybe Ast
processCall bs n as = case lookup n bs of
  Just (Lambda ps body) -> processLambda bs as ps body
  Just (Value v) -> Just (Value v)
  _ -> Nothing

evalAst :: Ast -> Bindings -> (Maybe Ast, Bindings)
evalAst (Define n v) bs = (Nothing, insert n v bs)
evalAst (Value v) bs = (Just (Value v), bs)
evalAst (Lambda p a) bs = (Just (Lambda p a), bs)
evalAst (Call n as) bs = (processCall bs n as, bs)
-- evalAst (Ast.Operator op as) bs = (processOperator (getOperator op :: BuiltinOperator Int) as bs >>= \v -> Just (Value (Int v)), bs)
