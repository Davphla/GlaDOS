module Lexer where
import Literal ( Literal(Int, Bool, Float, String, Array) )
import Parser.Parser
    ( FullParser(..),
      pEof,
      pWhitespaces,
      pAnySymbol, pSymbols, pComment,
       )
import Cpt ( Cpt(List, Literal, Identifier, Keyword), Keyword (Lambda, Else, Then, If) )
import Control.Applicative ( Alternative((<|>), some) )
import Parser.Litteral ( pBool, pInt, pFloat, pList, pLString )

keywords :: [Keyword]
keywords = [If, Then, Else, Lambda]

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
     <|> (Keyword . read <$> pSymbols (show <$> keywords))
     -- <|> (Operator . read <$> pStrings operator)
     <|> (List <$> pList pCpt)

startLexer :: Parser [Cpt]
startLexer = some (pComment *> pCpt <* (pWhitespaces <|> pEof)) <* pEof

