module Lexer where
import Literal ( Literal(Int, Bool, Float, String, Array) )
import Parser.Parser
    ( Parser,
      pEof,
      pWhitespaces,
      pAnySymbol, pSymbols, pComment, pStrings,
       )
import Cpt ( Cpt(List, Literal, Identifier, Keyword, Operator) )
import Control.Applicative ( Alternative((<|>), some) )
import Parser.Litteral ( pBool, pInt, pFloat, pList, pLString )

keywords :: [String]
keywords = ["if", "then", "else", "lambda"]

operator :: [String]
operator = [".", "+", "-", "*", "/", "`function`", "::", "->", "=", "$"]

pLitteral :: Parser Literal
pLitteral = (Int <$> pInt) 
  <|> (Float <$> pFloat) 
  <|> (Bool <$> pBool) 
  <|> (String <$> pLString) 
  <|> (Array <$> pList pLitteral) 

pCpt :: Parser Cpt
pCpt = (Literal <$> pLitteral)
     <|> (Identifier <$> pAnySymbol)
     <|> (Keyword <$> pSymbols keywords) 
     <|> (Operator <$> pStrings operator) 
     <|> (List <$> pList pCpt)

startLexer :: Parser [Cpt]
startLexer = some (pComment *> pCpt <* (pWhitespaces <|> pEof)) <* pEof

