{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Lexer.hs
-}

module Cpt.LexerParser (pCpt, startLexer, pCptExpression, pExpression, pPrototype, pAssignement, pLambda, pCptOperator, pCptKeyword, pCptLiteral, pCondition, pOperation) where
import LibParser.Parser
    (pEof,
      Parser(..), pStrings, pString, pAnySymbol, pWhitespaceWithNewLine, pAndAnd, pParenthesis, pManyWhitespace, pSomeWhitespace)
import Cpt.Cpt ( Cpt(Literal, Identifier, Operator, Keyword, Expression, Condition, Operation) )
import Control.Applicative ( Alternative((<|>), some) )
import LibParser.Literal ( pList, pLiteral )
import Data.Maybe ( fromJust )
import Cpt.Keyword ( strToKeywords )
import Cpt.Operator ( operatorFromStr, operators )

pCptKeyword :: String -> Parser Cpt
pCptKeyword str = Keyword . fromJust . strToKeywords <$> (pString str <* pSomeWhitespace)

pCptOperator :: Parser Cpt
pCptOperator = Operator . fromJust . operatorFromStr <$> pStrings operators

pCptLiteral :: Parser Cpt
pCptLiteral = Literal <$> pLiteral

pCptIdentifier :: Parser Cpt
pCptIdentifier = Identifier <$> pAnySymbol

pCptExpression :: Parser Cpt
pCptExpression = Expression <$> pList pOperand

pCondition :: Parser Cpt
pCondition = Condition <$> pAndAnd (pCptKeyword "if" *> pExpression) (pCptKeyword "then" *> pExpression) (pCptKeyword "else" *> pExpression)

pOperand :: Parser Cpt
pOperand = pCptLiteral <|> pCptIdentifier <|> pCptExpression

pOperation :: Parser Cpt
pOperation = Operation <$> pAndAnd pOperand pCptOperator pOperand

pExpression :: Parser Cpt
pExpression =  pCondition <|> pParenthesis pExpression <|> pLambda <|> pOperation <|> pCptLiteral

pLambda :: Parser Cpt
pLambda = pCptKeyword "lambda" >>= pure pExpression

pPrototype :: Parser Cpt
pPrototype = pCptIdentifier >>= pure pExpression

pAssignement :: Parser Cpt
pAssignement = (,) <$> pCptIdentifier <*> pList pOperand >>= pure pExpression

pCpt :: Parser Cpt
pCpt = pAssignement <|> pPrototype <* pWhitespaceWithNewLine

startLexer :: Parser [Cpt]
startLexer = some pCpt <* pEof
