{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- LexerParser.hs
-}

module Cpt.LexerParser (pCpt, startLexer, pPrototype, pAssignement, pLambda, pCptOperator, pCptKeyword, pCptLiteral, pCondition, pOperation, pExpression, pCptAnyOperator) where
import LibParser.Parser
    (pEof,
      Parser(..), pStrings, pString, pAnySymbol, pWhitespaceWithNewLine, pAndAnd, pParenthesis, pSomeWhitespace, pAnd, pManyWhitespace)
import Cpt.Cpt ( Cpt(Literal, Identifier, Operator, Keyword, Condition, Operation, Prototype, Assignement, Expression) )
import Control.Applicative ( Alternative((<|>), some) )
import LibParser.Literal ( pLiteral )
import Data.Maybe ( fromJust )
import Cpt.Keyword ( strToKeywords )
import Cpt.Operator ( operatorFromStr, operators )

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

pCondition :: Parser Cpt
pCondition = Condition <$> pAndAnd (pCptKeyword "if" *> pOperand) (pCptKeyword "then" *> pOperand) (pCptKeyword "else" *> pOperand)

pOperand :: Parser Cpt
pOperand = (pCptLiteral <|> pCptIdentifier <|> pParenthesis pExpression) <* pManyWhitespace

pOperation :: Parser Cpt
pOperation = Operation <$> some ((pOperand <|> pCptAnyOperator) <* pManyWhitespace)

pLambda :: Parser Cpt
pLambda = pCptKeyword "lambda" *> pExpression

pExpression :: Parser Cpt
pExpression = pCondition <|> pLambda <|> pOperation <|> pOperand

pPrototype :: Parser Cpt
pPrototype = Prototype <$> pAnd (pAnySymbol <* (pManyWhitespace >> pCptOperator "::")) (some (pAnySymbol <* pCptOperator "->"))

pAssignement :: Parser Cpt
pAssignement = Assignement <$> pAndAnd pAnySymbol (some pOperand <* pSomeWhitespace) pExpression

pCpt :: Parser Cpt
pCpt = pAssignement <|> pPrototype <* pWhitespaceWithNewLine

startLexer :: Parser [Cpt]
startLexer = some pCpt <* pEof
