{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Cpt.hs
-}

module Cpt (
    Cpt (Literal, Symbol, List),
    getSymbol, getLiteral, getList, printTree
  ) where

import Data.Maybe (fromJust)
import Literal (Literal)

data Cpt
  = Literal Literal
  | Symbol String
  | List [Cpt]
  deriving (Eq, Show)

getSymbol :: Cpt -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _ = Nothing

getLiteral :: Cpt -> Maybe Literal
getLiteral (Literal x) = Just x
getLiteral _ = Nothing

getList :: Cpt -> Maybe [Cpt]
getList (List l) = Just l
getList _ = Nothing

printTree :: Cpt -> Maybe String
printTree (Literal i) = Just $ "a Number " ++ show i
printTree (Symbol s) = Just $ "a Symbol '" ++ s ++ "'"
printTree (List [a]) = Just $ "a List with " ++ fromJust (printTree a)
printTree (List (a : as)) =
  Just $ "a List with "
      ++ fromJust (printTree a)
      ++ " followed by "
      ++ concatMap (\x -> fromJust (printTree x) ++ ", ") (init as)
      ++ fromJust (printTree $ last as)
printTree _ = Nothing
