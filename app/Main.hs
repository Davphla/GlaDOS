{-
-- EPITECH PROJECT, 2022
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Main.hs
-}

module Main (main) where
import Cpt.LexerParser (startLexer)
import LibParser.Parser (Parser(..))
import System.Exit (exitSuccess)
import Ast.Ast (cptToAst)


interpreteInput :: String -> String
interpreteInput str = case runParser startLexer str >>= (\(x, _) -> mapM cptToAst x) of
  Left err -> show err
  Right ast -> show ast -- TODO: add compilation here using the ast variable, which is a [Ast]

launchCmd :: String -> IO ()
launchCmd "quit" = exitSuccess
launchCmd str = putStrLn (interpreteInput str)

loop :: IO ()
loop = getLine >>= \line -> launchCmd line >> loop

main :: IO ()
main = loop
