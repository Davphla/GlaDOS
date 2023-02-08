{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Evaluation.hs
-}

module Evaluation (evalAst, Bindings, getOperator, getNeutral, removeNeutral, extractValue) where

import Ast (
    Operator (Plus, Minus, Times, Div),
    Ast (Define, Function, Value, Call, Operator)
  )
import Literal (Literal (Integer))
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)


-- -------------------------------- Bindings -------------------------------- --

type Bindings = Map String Ast

-- ---------------------------- Builtin operators --------------------------- --

data BuiltinOperator a = BuiltinOperator {
  function :: a -> a -> a
  , minArgs :: Int
  , neutral :: a
}

-- ------------------------------- Evaluation ------------------------------- --

getOperator :: Integral a => Operator -> BuiltinOperator a
getOperator Plus = BuiltinOperator {function = (+), minArgs = 0, neutral = 0}
getOperator Minus = BuiltinOperator {function = (-), minArgs = 1, neutral = 0}
getOperator Times = BuiltinOperator {function = (*), minArgs = 0, neutral = 1}
getOperator Div = BuiltinOperator {function = div, minArgs = 1, neutral = 1}

-- processOperator :: Integral a => BuiltinOperator a -> [Ast] -> Bindings -> Maybe a
-- processOperator op as bs
--   | length as < minArgs op = Nothing
--   | otherwise = Just (foldr (function op) (neutral op)
--     (fromJust (mapM (extractValue . fromJust . fst . (`evalAst` bs)) as)))

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
extractValue (Value (Integer i)) = Just $ fromIntegral i
extractValue _ = Nothing

functionBindings :: Bindings -> [Ast] -> [String] -> Maybe Bindings
functionBindings bs [] [] = Just bs
functionBindings bs (a:as) (n:ns) = functionBindings (insert n a bs) as ns
functionBindings _ _ _ = Nothing

processFunction :: Bindings -> [Ast] -> [String] -> Ast -> Maybe Ast
processFunction bs as ns body = functionBindings bs as ns >>= fst . evalAst body

processCall :: Bindings -> String -> [Ast] -> Maybe Ast
processCall bs n as = case lookup n bs of
  Just (Function ps body) -> processFunction bs as ps body
  Just (Value v) -> Just (Value v)
  _ -> Nothing

evalAst :: Ast -> Bindings -> (Maybe Ast, Bindings)
evalAst (Define n v) bs = (Nothing, insert n v bs)
evalAst (Value v) bs = (Just (Value v), bs)
evalAst (Function p a) bs = (Just (Function p a), bs)
evalAst (Call n as) bs = (processCall bs n as, bs)
evalAst (Operator op as) bs = (processOperator (getOperator op :: BuiltinOperator Int) as bs >>= \v -> Just (Value (Integer v)), bs)