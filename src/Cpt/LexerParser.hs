{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Lexer.hs
-}

module Cpt.LexerParser (pCpt, startLexer, pCptExpression, pPrototype, pAssignement, pLambda, pCptOperator, pCptKeyword, pCptLiteral, pCondition, pOperation, pExpression, pCptAnyOperator) where
import LibParser.Parser
    (pEof,
      Parser(..), pStrings, pString, pAnySymbol, pWhitespaceWithNewLine, pAndAnd, pParenthesis, pSomeWhitespace, pAnd, pManyWhitespace)
import Cpt.Cpt ( Cpt(Literal, Identifier, Operator, Keyword, Expression, Condition, Operation, Prototype, Assignement) )
import Control.Applicative ( Alternative((<|>), some) )
import LibParser.Literal ( pList, pLiteral )
import Data.Maybe ( fromJust )
import Cpt.Keyword ( strToKeywords )
import Cpt.Operator ( operatorFromStr, operators )
import Control.Monad (void)

pCptKeyword :: String -> Parser Cpt
pCptKeyword str = Keyword . fromJust . strToKeywords <$> (pString str <* pSomeWhitespace)

pCptOperator :: String -> Parser Cpt
pCptOperator str = Operator . fromJust . operatorFromStr <$> (pString str <* pManyWhitespace)

pCptAnyOperator :: Parser Cpt
pCptAnyOperator = Operator . fromJust . operatorFromStr <$> (pStrings operators <* pManyWhitespace)

pCptLiteral :: Parser Cpt
pCptLiteral = Literal <$> pLiteral

pCptIdentifier :: Parser Cpt
pCptIdentifier = Identifier <$> pAnySymbol

pCptExpression :: Parser Cpt
pCptExpression = Expression <$> pList pOperand

pCondition :: Parser Cpt
pCondition = Condition <$> pAndAnd (pCptKeyword "if" *> pExpression) (pCptKeyword "then" *> pExpression) (pCptKeyword "else" *> pExpression)

pOperand :: Parser Cpt
pOperand = (pCptLiteral <|> pCptIdentifier <|> pCptExpression) <* pManyWhitespace

pOperation :: Parser Cpt
pOperation = Operation <$> some ((pOperand <|> pParenthesis pExpression <|> pCptAnyOperator) <* pManyWhitespace)

pLambda :: Parser Cpt
pLambda = pCptKeyword "lambda" *> pExpression

pExpression :: Parser Cpt
pExpression =  pCondition <|> pParenthesis pExpression <|> pLambda <|> pOperation <|> pOperand

pPrototype :: Parser Cpt
pPrototype = Prototype <$> pAnd (pAnySymbol <* (pManyWhitespace >> pCptOperator "::")) (some (pAnySymbol <* pCptOperator "->"))

pAssignement :: Parser Cpt
pAssignement = Assignement <$> pAndAnd pAnySymbol (some pOperand <* pSomeWhitespace) pExpression

pCpt :: Parser Cpt
pCpt = pAssignement <|> pPrototype <* pWhitespaceWithNewLine

startLexer :: Parser [Cpt]
startLexer = some pCpt <* pEof
