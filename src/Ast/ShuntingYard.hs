module Ast.ShuntingYard (shuntingYard) where

import Cpt.Cpt (Cpt (Literal, Operation, Operator))
import Cpt.Operator (Operator (..))
import Error (GladosError)

import GHC.Generics (Associativity (LeftAssociative))

precedence :: Cpt -> Int
precedence (Cpt.Cpt.Operator (Cpt.Operator.Operator _ p _)) = p
precedence _ = 0

associativity :: Cpt -> Associativity
associativity (Cpt.Cpt.Operator (Cpt.Operator.Operator _ _ a)) = a
associativity _ = LeftAssociative

-- Kept for debug purposes
-- ioShuntingYard' :: [Cpt] -> [Cpt] -> [Cpt] -> IO (Either [GladosError] [Cpt])
-- ioShuntingYard' out ops [] = return $ Right $ out ++ ops
-- ioShuntingYard' out ops ((Cpt.Cpt.Literal v):ts) =
--  ioShuntingYard' (out ++ [Cpt.Cpt.Literal v]) ops ts
-- ioShuntingYard' out ops ((Cpt.Cpt.Operation ls):ts) =
--  print ops >> ioShuntingYard' [] [] ls
--   >>= (\(Right xs) -> ioShuntingYard' (out ++ xs) ops ts)
-- ioShuntingYard' out ops (op:ts) = print ops >> ioShuntingYard' newOut newOps ts where
--   (topOps, restOps) = span (\c -> precedence c > precedence op ||
--     (precedence c == precedence op && associativity c == LeftAssociative)) ops
--   newOut = out ++ topOps
--   newOps = op : restOps

shuntingYard' :: [Cpt] -> [Cpt] -> [Cpt] -> Either [GladosError] [Cpt]
shuntingYard' out ops [] = Right $ out ++ ops
shuntingYard' out ops ((Cpt.Cpt.Literal v):ts) =
  shuntingYard' (out ++ [Cpt.Cpt.Literal v]) ops ts
shuntingYard' out ops ((Cpt.Cpt.Operation ls):ts) =
  shuntingYard' [] [] ls >>= (\xs -> shuntingYard' (out ++ xs) ops ts)
shuntingYard' out ops (op:ts) = shuntingYard' newOut newOps ts where
  (topOps, restOps) = span (\c -> precedence c > precedence op ||
    (precedence c == precedence op && associativity c == LeftAssociative)) ops
  newOut = out ++ topOps
  newOps = op : restOps

shuntingYard :: [Cpt] -> Either [GladosError] [Cpt]
shuntingYard = shuntingYard' [] []
