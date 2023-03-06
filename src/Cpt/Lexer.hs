{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Lexer.hs
-}

module Cpt.Lexer (pCpt, startLexer, pExpression) where
import LibParser.Parser
    (pEof,
      Parser(..), pStrings, pManyWhitespace, pString, pAnySymbol)
import Cpt.Cpt ( Cpt(Literal, Identifier, Operator, Keyword) )
import Control.Applicative ( Alternative((<|>), some) )
import LibParser.Literal ( pList, pLiteral )
import Data.Maybe ( fromJust )
import Cpt.Keyword ( strToKeywords )
import Cpt.Operator ( operatorFromStr, operators )

pKeyword :: String -> Parser Cpt
pKeyword str = Keyword . fromJust . strToKeywords <$> pString str

pOperator :: Parser Cpt
pOperator = Cpt.Operator . fromJust . operatorFromStr <$> pStrings operators

pExpression :: Parser Cpt
pExpression = pCondition

pCondition :: Parser Cpt
pCondition = pKeyword "if" >>= pExpression >>= pKeyword "then" >>= pExpression >>= pKeyword "else" >>= pExpression

pCpt :: Parser Cpt
pCpt = pKeyword <|> (pOperator <|> (pManyWhitespace *> pOperator)) <|> (pOperand <|> (pManyWhitespace *> pOperand))

pOperand :: Parser Cpt
pOperand = (Literal <$> pLiteral)
  <|> (Identifier <$> pIdentifier)
  <|> (List <$> pList pOperand)

pIdentifier :: Parser String
pIdentifier = Identifier <$> pAnySymbol

pLambda :: Parser Cpt
pLambda = pKeyword "lambda" >>= pExpression

pPrototype :: Parser Cpt
pPrototype = pIdentifier >>= pExpression

pAssignement :: Parser Cpt
pAssignement = (,) <$> pIdentifier <*> pList pOperand >>= pExpression

startLexer :: Parser [Cpt]
startLexer = some pCpt <* pEof
