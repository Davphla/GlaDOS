module Lexer (pLitteral, pCpt, startLexer) where
import Literal ( Literal(Int, Bool, Float, String, Array) )
import Parser.Parser
    (pEof,
      pWhitespaces,
      pAnySymbol,
      pSymbols,
      Parser(..))
import Cpt ( Cpt(List, Literal, Identifier, Keyword), Keyword (Lambda, Else, Then, If) )
import Control.Applicative ( Alternative((<|>), some) )
import Parser.Litteral ( pBool, pInt, pFloat, pList, pLString )
import Data.Maybe

strToKeywords :: String -> Maybe Keyword
strToKeywords "if" = Just If
strToKeywords "then" = Just Then
strToKeywords "else" = Just Else
strToKeywords "lambda" = Just Lambda
strToKeywords _ = Nothing

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
     <|> (Keyword . fromJust . strToKeywords <$> pSymbols keywords)
     -- <|> (Operator . read <$> pStrings operator)
     <|> (Identifier <$> pAnySymbol)
     <|> (List <$> pList pCpt)

startLexer :: Parser [Cpt]
startLexer = some (pCpt <* (pWhitespaces <|> pEof)) <* pEof

