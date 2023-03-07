{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Parser.hs
-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module LibParser.Parser (Parser, satisfy, skip, pEof, pChar, sChar, pChars, pString, pStrings, pAnySymbol, pEncloseBySpecificParser, pEncloseByParser, pSymbol, pSymbols, pComment, runParser, pWhitespace, pManyWhitespace, pSomeWhitespace, pWhitespaceWithNewLine, pAnd, pAndAnd, pParenthesis, pAnyChar) where
import Control.Applicative ( Alternative(empty, (<|>), many, some) )
import Data.List ( nub )
import Control.Monad (void)
import Error (GladosError (Parser), ParseError (..))

-- type Infos = (Int, Int)
--- line and column


newtype Parser a = Parser {
  runParser :: String ->  Either [GladosError] (a, String)
}

-- type Parser = FullParser Infos

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (LibParser.Parser.Parser p) = LibParser.Parser.Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = LibParser.Parser.Parser $ \input -> Right (a, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  LibParser.Parser.Parser f <*> LibParser.Parser.Parser p = LibParser.Parser.Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  LibParser.Parser.Parser p >>= k = LibParser.Parser.Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Alternative Parser where
  empty :: Parser a
  empty = LibParser.Parser.Parser $ \_ -> Left [Error.Parser InvalidSynthax]

  (<|>) :: Parser a -> Parser a -> Parser a
  LibParser.Parser.Parser l <|> LibParser.Parser.Parser r = LibParser.Parser.Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)



satisfy :: (Char -> Bool) -> Parser Char
satisfy f = LibParser.Parser.Parser $ \case
    [] -> Left [Error.Parser UnexpectedEnd]
    x:xs
      | f x -> Right (x,xs)
      | otherwise -> Left [Error.Parser InvalidSynthax]

skip :: (Char -> Bool) -> Parser ()
skip f = void $ satisfy f

pEof :: Parser ()
pEof = LibParser.Parser.Parser $ \case
    [] -> Right ((), [])
    _ -> Left [Error.Parser UnexpectedEnd]

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
pWhitespace = void $ satisfy (`elem` " \t")

pWhitespaceWithNewLine :: Parser ()
pWhitespaceWithNewLine = void $ satisfy (`elem` " \t\n")

pManyWhitespace :: Parser ()
pManyWhitespace = void $ many pWhitespace

pSomeWhitespace :: Parser ()
pSomeWhitespace = void $ some pWhitespace

pAnyChar :: Parser Char
pAnyChar = pChars (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

pAnySymbol :: Parser String
pAnySymbol = some $ pChars (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

pParenthesis :: Parser a -> Parser a
pParenthesis = pEncloseBySpecificParser (void $ pChar '(') (void $ pChar ')')

pEncloseBySpecificParser :: Parser () -> Parser () -> Parser a -> Parser a
pEncloseBySpecificParser pIn pOut p = pIn *> p <* pOut

pEncloseByParser :: Parser () -> Parser a -> Parser a
pEncloseByParser pEnclose  = pEncloseBySpecificParser pEnclose pEnclose

pSymbol :: String -> Parser String
pSymbol str = pEncloseByParser pSomeWhitespace (pString str)

pSymbols :: [String] -> Parser String
pSymbols = foldr1 (<|>) . fmap pSymbol

pComment :: Parser ()
pComment = void $ pString "--" *> many (satisfy (/= '\n')) <* (void (pChar '\n') <|> pEof)

pAnd :: Parser a -> Parser b -> Parser (a, b)
pAnd p1 p2 = (,) <$> p1 <*> p2

pAndAnd :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
pAndAnd p1 p2 p3 = (,,) <$> p1 <*> p2 <*> p3
