module Parser.Litteral where
import Parser.Parser ( Parser, pChars, pParenthesis, pSymbol, sChar, pWhitespaces, pString, pEncloseByParser, pAnySymbol )
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

