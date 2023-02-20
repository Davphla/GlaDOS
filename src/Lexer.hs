module Lexer where
import Literal ( Literal(Int, Bool, Float) )
import Parser.Parser
    ( Parser,
      pEof,
      pWhitespaces,
      pAnySymbol,
      pInt,
      pBool,
      pFloat,
      pList )
import Cpt ( Cpt(List, Literal, Identifier) )
import Control.Applicative ( Alternative((<|>), some) )

pBool :: Parser Bool
pBool = (True <$ pSymbol "#t") <|> (False <$ pSymbol "#f")

pLitteral :: Parser Literal
pLitteral = (Bool <$> pBool) <|> ((Float <$> pFloat) <|> (Int <$> pInt))

pCpt :: Parser Cpt
pCpt = (Literal <$> pLitteral) <|> (Identifier <$> pAnySymbol) <|> (List <$> pList pCpt)

pLisp :: Parser [Cpt]
pLisp = some (pCpt <* (pWhitespaces <|> pEof)) <* pEof
