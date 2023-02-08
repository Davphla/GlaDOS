{-
-- EPITECH PROJECT, 2022
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Main.hs
-}

module Main (main) where
import Lexer (pLisp, runParser)
import System.Exit ( exitSuccess )
import Ast ( cptToAst )

interpreteInput :: String -> String
interpreteInput str = case runParser pLisp str of
  Left err -> show err
  Right (cpt, _) -> show (cptToAst <$> cpt)

prompt :: String
prompt = "> "

launchCmd :: String -> IO ()
launchCmd "quit" = exitSuccess
launchCmd str = putStr prompt >> putStrLn (interpreteInput str)

loop :: IO ()
loop = getLine >>= \line -> launchCmd line >> loop

main :: IO ()
main = loop
