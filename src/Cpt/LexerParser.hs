{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- LexerParser.hs
-}

module Cpt.LexerParser (pCpt, startLexer, pPrototype, pAssignement, pLambda, pCondition, pOperation, pExpression, pIdentifier, pParameter) where
import LibParser.Parser
    (pEof,
      Parser(..), pAndAnd, pParenthesis, pAnd, pManyWhitespace, pChars, sChar, pWhitespaceWithNewLine, pComment)
import Cpt.Cpt ( Cpt(Condition, Operation, Prototype, Assignement, Identifier), pCptKeyword, pCptLiteral, pCptAnyOperator, pCptOperator )
import Control.Applicative ( Alternative((<|>), some, many) )


pIdentifier :: Parser String
pIdentifier = (:) <$> pChars (['a'..'z'] ++ ['A'..'Z']) <*> many (pChars (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']))

pCondition :: Parser Cpt
pCondition = Condition <$> pAndAnd (pCptKeyword "if" *> pOperand) (pCptKeyword "then" *> pOperand) (pCptKeyword "else" *> pOperand)

pOperand :: Parser Cpt
pOperand = (pCptLiteral <|> (Identifier <$> pIdentifier) <|> pParenthesis pExpression) <* pManyWhitespace

pParameter :: Parser Cpt
pParameter = (pCptLiteral <|> (Identifier <$> pIdentifier)) <* pManyWhitespace

pOperation :: Parser Cpt
pOperation = Operation <$> some ((pOperand <|> pCptAnyOperator) <* pManyWhitespace)

pLambda :: Parser Cpt
pLambda = pCptKeyword "lambda" *> pParenthesis pExpression

pKeywordExpression :: Parser Cpt
pKeywordExpression = pCondition <|> pLambda

pExpression :: Parser Cpt
pExpression = pKeywordExpression <|> pOperation

pPrototype :: Parser Cpt
pPrototype = Prototype <$> pAnd (pIdentifier <* (pManyWhitespace >> pCptOperator "::")) (some ((pIdentifier <* pManyWhitespace <* pCptOperator "->") <|> pIdentifier))

pAssignement :: Parser Cpt
pAssignement = Assignement <$> pAndAnd (pIdentifier <* pManyWhitespace) (many pParameter <|> pure []) (sChar '=' >> pManyWhitespace *> pExpression)

pCpt :: Parser Cpt
pCpt = (pAssignement <|> pPrototype) <* many (pWhitespaceWithNewLine <|> pComment)

startLexer :: Parser [Cpt]
startLexer = some pCpt <* pEof
