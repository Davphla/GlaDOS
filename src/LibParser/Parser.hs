{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Evaluation.hs
-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module LibParser.Parser (Parser, satisfy, skip, pEof, pChar, sChar, pChars, pString, pStrings, pAnySymbol, pParenthesis, pEncloseByParser, pSymbol, pSymbols, pComment, runParser, pWhitespace, pManyWhitespace, pSomeWhitespace) where
import Control.Applicative ( Alternative(empty, (<|>), many, some) )
import Data.List ( nub )
import Control.Monad (void)

-- type Infos = (Int, Int)
--- line and column

data ParseError = InvalidSynthax
  | Unexpected
  | UnexpectedEnd
  deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String ->  Either [ParseError] (a, String)
}

-- type Parser = FullParser Infos

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input 
    runParser (k output) rest

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left [InvalidSynthax]

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)



satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
    [] -> Left [UnexpectedEnd]
    x:xs
      | f x -> Right (x,xs)
      | otherwise -> Left [InvalidSynthax]

skip :: (Char -> Bool) -> Parser ()
skip f = void $ satisfy f

pEof :: Parser ()
pEof = Parser $ \case
    [] -> Right ((), [])
    _ -> Left [UnexpectedEnd]

pChar :: Char -> Parser Char
pChar h = satisfy (== h)

sChar :: Char -> Parser ()
sChar h = skip (== h)

pChars :: String -> Parser Char
pChars s = satisfy (`elem` s)

pString :: String -> Parser String
pString = traverse pChar

pStrings :: [String] -> Parser String
pStrings = foldr1 (<|>) . fmap pString 

pWhitespace :: Parser ()
pWhitespace = void $ satisfy (`elem` " \n\t")

pManyWhitespace :: Parser ()
pManyWhitespace = void $ many pWhitespace

pSomeWhitespace :: Parser ()
pSomeWhitespace = void $ some pWhitespace

pAnySymbol :: Parser String
pAnySymbol = some $ pChars (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

pParenthesis :: Parser () -> Parser () -> Parser a -> Parser a
pParenthesis pIn pOut p = pIn *> p <* pOut

pEncloseByParser :: Parser () -> Parser a -> Parser a
pEncloseByParser pEnclose  = pParenthesis pEnclose pEnclose

pSymbol :: String -> Parser String
pSymbol str = pEncloseByParser pSomeWhitespace (pString str) 

pSymbols :: [String] -> Parser String
pSymbols = foldr1 (<|>) . fmap pSymbol 

pComment :: Parser ()
pComment = void $ pString "--" *> many (satisfy (/= '\n')) <* (void (pChar '\n') <|> pEof)

