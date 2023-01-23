module Cpt (Cpt (Integer, Symbol, List)) where

import Data.Maybe (fromJust)

data Cpt
  = Integer Int
  | Symbol String
  | List [Cpt]

getSymbol :: Cpt -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _ = Nothing

getInteger :: Cpt -> Maybe Int
getInteger (Integer x) = Just x
getInteger _ = Nothing

getList :: Cpt -> Maybe [Cpt]
getList (List l) = Just l
getList _ = Nothing

printTree :: Cpt -> Maybe String
printTree (Integer i) = Just $ "a Number " ++ show i
printTree (Symbol s) = Just $ "a Symbol '" ++ s ++ "'"
printTree (List [a]) = Just $ "a List with " ++ fromJust (printTree a)
printTree (List (a : as)) =
  Just $
    "a List with "
      ++ fromJust (printTree a)
      ++ " followed by "
      ++ concatMap (\x -> fromJust (printTree x) ++ ", ") (init as)
      ++ fromJust (printTree $ last as)
printTree _ = Nothing
