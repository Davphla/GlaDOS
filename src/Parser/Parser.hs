{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Evaluation.hs
-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Lexer (Parser,
    runParser,
    satisfy,
    pChar,
    pChars,
    pSymbol,
    pWhitespaces,
    pUInt,
    pInt,
    pParenthesis,
    pPair,
    pList,
    pBool,
    pFloat,
    pLitteral,
    pAnySymbol,
    pCpt,
    pLisp,
  ) where
import Control.Applicative ( Alternative(empty, (<|>), many, some) )
import Data.List ( nub )
import Literal
import Cpt
import Control.Monad (void)

type Infos = (Int, Int)
--- line and column

data ParseError infos = InvalidSynthax
  | Unexpected
  | UnexpectedEnd
  deriving (Show, Eq)

newtype FullParser infos a = Parser {
  runParser :: String ->  Either [ParseError infos] (a, String)
}

type Parser = FullParser Infos

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

pEof :: Parser ()
pEof = Parser $ \case
    [] -> Right ((), [])
    _ -> Left [UnexpectedEnd]


pChar :: Char -> Parser Char
pChar h = satisfy (== h)

pChars :: String -> Parser Char
pChars s = satisfy (`elem` s)

pAnySymbol :: Parser String
pAnySymbol = some $ pChars (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['+', '-', '*', '/'])

pUInt :: Parser Int
pUInt = read <$> some (pChars ['0'..'9'])

pParenthesis :: Parser a -> Parser a
pParenthesis p = pChar '(' *> p <* pChar ')'

pWhitespaces :: Parser ()
pWhitespaces = void $ some (satisfy (`elem` " \n\t"))

pPair :: Parser a -> Parser (a, a)
pPair p = pParenthesis $ (,) <$> p <*> (pChar ',' *> p)

pInt :: Parser Int
pInt = (negate <$> (pChar '-' *> pUInt)) <|> pUInt

pBool :: Parser Bool
pBool = (True <$ pSymbol "#t") <|> (False <$ pSymbol "#f")

pFloat :: Parser Double
pFloat = pInt >>= \i -> fromIntegral i <$ pChar '.'

pLitteral :: Parser Literal
pLitteral = (Bool <$> pBool) <|> ((Float <$> pFloat) <|> (Int <$> pInt))

pList :: Parser a -> Parser [a]
pList p = pParenthesis $ (:) <$> p <*> many (pWhitespaces *> p)

pSymbol :: String -> Parser String
pSymbol = traverse pChar

pCpt :: Parser Cpt
pCpt = (Literal <$> pLitteral) <|> (Identifier <$> pAnySymbol) <|> (List <$> pList pCpt)

pLisp :: Parser [Cpt]
pLisp = some (pCpt <* (pWhitespaces <|> pEof)) <* pEof
