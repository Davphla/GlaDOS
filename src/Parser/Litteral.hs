{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Evaluation.hs
-}

module Parser.Litteral (pBool, pUInt, pInt, pFloat, pPair, pList, pLString) where

import Parser.Parser
    (sChar,
      pChars,
      pWhitespaces,
      pAnySymbol,
      pParenthesis,
      pEncloseByParser,
      pSymbol, Parser )
import Control.Applicative ( Alternative((<|>), some, many) )


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
pList p = pParenthesis (sChar '[') (sChar ']') ((:) <$> p <*> many (pWhitespaces *> p))

pLString :: Parser String
pLString = pEncloseByParser (sChar '"') pAnySymbol
