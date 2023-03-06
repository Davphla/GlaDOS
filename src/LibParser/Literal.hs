{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Evaluation.hs
-}

module LibParser.Literal (pBool, pUInt, pInt, pFloat, pPair, pList, pLString, pLiteral) where

import LibParser.Parser
    (sChar,
      pChars,
      pAnySymbol,
      pParenthesis,
      pEncloseByParser,
      pSymbol, Parser, pManyWhitespace )
import Control.Applicative ( Alternative((<|>), some, many) )
import Cpt.Literal ( Literal(Array, Int, Float, Bool, String) )


pBool :: Parser Bool
pBool = (True <$ pSymbol "True" ) <|> (False <$ pSymbol "False")

pUInt :: Parser Int
pUInt = read <$> some (pChars ['0'..'9'])

pInt :: Parser Int
pInt = (negate <$> (sChar '-' *> pUInt)) <|> pUInt

pFloat :: Parser Double
pFloat = pInt >>= \i -> fromIntegral i <$ sChar '.'

pPair :: Parser a -> Parser (a, a)
pPair p = pParenthesis (sChar '(') (sChar ')') ((,) <$> p <*> (sChar ',' *> p))

pList :: Parser a -> Parser [a]
pList p = pParenthesis (sChar '[') (sChar ']') ((:) <$> p <*> many (pManyWhitespace *> p))

pLString :: Parser String
pLString = pEncloseByParser (sChar '"') pAnySymbol

pLiteral :: Parser Literal
pLiteral = (Int <$> pInt)
  <|> (Float <$> pFloat)
  <|> (Bool <$> pBool)
  <|> (String <$> pLString)
  <|> (Array <$> pList pLiteral)