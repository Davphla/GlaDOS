{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Literal.hs
-}

module LibParser.Literal (pBool, pUInt, pInt, pFloat, pPair, pList, pLString, pLiteral, pLChar) where

import LibParser.Parser
    (sChar,
      pChars,
      pAnySymbol,
      pParenthesis,
      pEncloseByParser,
      pSymbol, Parser, pManyWhitespace, pEncloseBySpecificParser, pAnyChar )
import Control.Applicative ( Alternative((<|>), some, many) )
import Cpt.Literal ( Literal(Array, Int, Float, Bool, String, Char) )


pBool :: Parser Bool
pBool = (True <$ pSymbol "True" ) <|> (False <$ pSymbol "False")

pUInt :: Parser Int
pUInt = read <$> some (pChars ['0'..'9'])

pInt :: Parser Int
pInt = (negate <$> (sChar '-' *> pUInt)) <|> pUInt

pFloat :: Parser Double
pFloat = pInt >>= \i -> fromIntegral i <$ sChar '.'

pPair :: Parser a -> Parser (a, a)
pPair p = pParenthesis ((,) <$> p <*> (sChar ',' *> p))

pList :: Parser a -> Parser [a]
pList p = pEncloseBySpecificParser (sChar '[') (sChar ']') ((:) <$> p <*> many (pManyWhitespace *> p))

pLChar :: Parser Char
pLChar = pEncloseByParser (sChar '\'') pAnyChar

pLString :: Parser String
pLString = pEncloseByParser (sChar '"') pAnySymbol

pLiteral :: Parser Literal
pLiteral = (Int <$> pInt)
  <|> (Float <$> pFloat)
  <|> (Bool <$> pBool)
  <|> (Char <$> pLChar)
  <|> (String <$> pLString)
  <|> (Array <$> pList pLiteral)