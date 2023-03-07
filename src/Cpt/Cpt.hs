{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Cpt.hs
-}

module Cpt.Cpt (
    Assignement,
    Cpt (..),
    Condition,
    Expression,
    Operation,
    Lambda,
    getIdentifier, getKeyword, getLiteral, getExpression, getOperator, pCptKeyword, pCptOperator, pCptAnyOperator, pCptLiteral, Identifier
  ) where

import Cpt.Literal (Literal)
import Cpt.Operator (Operator, operatorFromStr, operators)
import Cpt.Keyword (Keyword, strToKeywords)
import Error (GladosError (Cpt), CptError (InvalidCpt), CptErrorReason (..))
import LibParser.Parser
import Data.Maybe
import LibParser.Literal

type Identifier = String
type Expression = [Cpt]
type Condition = (Cpt, Cpt, Cpt)
type Operation = [Cpt]
type Assignement = (Identifier, [Cpt], Cpt)
type Prototype = (Identifier, [Identifier])
type Lambda = [Cpt]

data Cpt
  = Literal Literal
  | Identifier String
  | Keyword Keyword
  | Operator Operator
  | Expression Expression
  | Operation Expression
  | Condition Condition
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
  show (Operation (l:ls)) = "Operation " ++ foldl (\x acc -> x ++ " " ++ acc) (show l) (map show ls)
  show (Operation []) = "empty Cpt"
  show (Condition (a, b, c)) = "{if} " ++ show a ++ " {then} " ++ show b ++ " {else} " ++ show c
  show (Assignement (s, l, c)) = show s ++ " " ++ show l ++ " = " ++ show c
  show (Prototype (s, l)) = show s ++ " " ++ show l
  show (Lambda l) = "lambda " ++ show l

getIdentifier :: Cpt -> Either [GladosError] String
getIdentifier (Identifier s) = Right s
getIdentifier c = Left [Cpt $ InvalidCpt InvalidCptNotIdentifier $ show c]

getLiteral :: Cpt -> Either [GladosError] Literal
getLiteral (Literal x) = Right x
getLiteral c = Left [Cpt $ InvalidCpt InvalidCptNotLiteral $ show c]

getKeyword :: Cpt -> Either [GladosError] Keyword
getKeyword (Keyword k) = Right k
getKeyword c = Left [Cpt $ InvalidCpt InvalidCptNotKeyword $ show c]

getOperator :: Cpt -> Either [GladosError] Operator
getOperator (Operator o) = Right o
getOperator c = Left [Cpt $ InvalidCpt InvalidCptNotOperator $ show c]

getExpression :: Cpt -> Either [GladosError] [Cpt]
getExpression (Expression l) = Right l
getExpression c = Left [Cpt $ InvalidCpt InvalidCptNotExpression $ show c]


pCptKeyword :: String -> Parser Cpt
pCptKeyword str = Keyword . fromJust . strToKeywords <$> (pString str <* pSomeWhitespace)

pCptOperator :: String -> Parser Cpt
pCptOperator str = Operator . fromJust . operatorFromStr <$> (pString str <* pManyWhitespace)

pCptAnyOperator :: Parser Cpt
pCptAnyOperator = Operator . fromJust . operatorFromStr <$> (pStrings operators <* pManyWhitespace)

pCptLiteral :: Parser Cpt
pCptLiteral = Literal <$> pLiteral

-- Faire une fonction qui crée l'abre correspondant à une expression en
-- ajoutant les priorités.

-- Les évaluations sont faites au moment de la création de l'Ast, si
-- l'expression est évaluable, sinon elle est simplement stockée dans
-- la table des symboles.