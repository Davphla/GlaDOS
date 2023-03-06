{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Lexer.hs
-}

module Cpt.LexerParser (pCpt, startLexer, pExpression, pPrototype, pAssignement, pLambda, pCptOperator, pCptKeyword, pCptLiteral, pCondition) where
import LibParser.Parser
    (pEof,
      Parser(..), pStrings, pString, pAnySymbol, pManyWhitespace)
import Cpt.Cpt ( Cpt(Literal, Identifier, Operator, Keyword, Expression) )
import Control.Applicative ( Alternative((<|>), some) )
import LibParser.Literal ( pList, pLiteral )
import Data.Maybe ( fromJust )
import Cpt.Keyword ( strToKeywords )
import Cpt.Operator ( operatorFromStr, operators )

pCptKeyword :: String -> Parser Cpt
pCptKeyword str = Keyword . fromJust . strToKeywords <$> pString str

pCptOperator :: Parser Cpt
pCptOperator = Operator . fromJust . operatorFromStr <$> pStrings operators

pCptLiteral :: Parser Cpt
pCptLiteral = Literal <$> pLiteral

pCptIdentifier :: Parser Cpt
pCptIdentifier = Identifier <$> pAnySymbol

cptExpression :: Parser Cpt
cptExpression = Expression <$> pList pOperand


pCondition :: Parser Cpt
pCondition = pCptKeyword "if" >>= pure pExpression >>= pure (pCptKeyword "then") >>= pure pExpression >>= pure (pCptKeyword "else") >>= pure pExpression

pExpression :: Parser Cpt
pExpression = (pCondition
  <|> pLambda
  <|> pCptLiteral) <* pManyWhitespace

pOperand :: Parser Cpt
pOperand = pCptLiteral <|> pCptIdentifier <|> cptExpression

pLambda :: Parser Cpt
pLambda = pCptKeyword "lambda" >>= pure pExpression

pPrototype :: Parser Cpt
pPrototype = pCptIdentifier >>= pure pExpression

pAssignement :: Parser Cpt
pAssignement = (,) <$> pCptIdentifier <*> pList pOperand >>= pure pExpression

pCpt :: Parser Cpt
pCpt = pAssignement <|> pPrototype

startLexer :: Parser [Cpt]
startLexer = some pCpt <* pEof
