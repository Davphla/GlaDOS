{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Lexer.hs
-}

module Cpt.LexerParser (pCpt, startLexer, pExpression, pPrototype, pAssignement, pLambda, cptOperator) where
import LibParser.Parser
    (pEof,
      Parser(..), pStrings, pManyWhitespace, pString, pAnySymbol)
import Cpt.Cpt ( Cpt(Literal, Identifier, Operator, Keyword, Expression) )
import Control.Applicative ( Alternative((<|>), some) )
import LibParser.Literal ( pList, pLiteral )
import Data.Maybe ( fromJust )
import Cpt.Keyword ( strToKeywords )
import Cpt.Operator ( operatorFromStr, operators )

cptKeyword :: String -> Parser Cpt
cptKeyword str = Keyword . fromJust . strToKeywords <$> pString str

cptOperator :: Parser Cpt
cptOperator = Operator . fromJust . operatorFromStr <$> pStrings operators

pExpression :: Parser Cpt
pExpression = pCondition

pCondition :: Parser Cpt
pCondition = cptKeyword "if" >>= pure pExpression >>= pure (cptKeyword "then") >>= pure pExpression >>= pure (cptKeyword "else") >>= pure pExpression

cptLiteral :: Parser Cpt
cptLiteral = Literal <$> pLiteral

cptIdentifier :: Parser Cpt
cptIdentifier = Identifier <$> pAnySymbol

cptExpression :: Parser Cpt
cptExpression = Expression <$> pList pOperand


pOperand :: Parser Cpt
pOperand = cptLiteral <|> cptIdentifier <|> cptExpression

pLambda :: Parser Cpt
pLambda = cptKeyword "lambda" >>= pure pExpression

pPrototype :: Parser Cpt
pPrototype = cptIdentifier >>= pure pExpression

pAssignement :: Parser Cpt
pAssignement = (,) <$> cptIdentifier <*> pList pOperand >>= pure pExpression

pCpt :: Parser Cpt
pCpt = pAssignement <|> pPrototype


startLexer :: Parser [Cpt]
startLexer = some pCpt <* pEof
