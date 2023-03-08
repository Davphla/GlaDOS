{-
-- EPITECH PROJECT, 2022
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Main.hs
-}

module Main (main) where
import Llvm.Llvm (compileModuleToObj)
import Cpt.LexerParser (startLexer)
import LibParser.Parser (Parser(..))
import System.Exit (exitSuccess)
import Ast.Ast (cptToAst)


interpreteInput :: String -> IO ()
interpreteInput str = case runParser startLexer str >>= (\(x, _) -> mapM cptToAst x) of
  Left err -> print err
  Right ast -> compileModuleToObj ast

launchCmd :: String -> IO ()
launchCmd "quit" = exitSuccess
launchCmd str = interpreteInput str

loop :: IO ()
loop = getLine >>= \line -> launchCmd line >> loop

main :: IO ()
main = loop
