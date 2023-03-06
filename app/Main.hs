{-
-- EPITECH PROJECT, 2022
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Main.hs
-}

module Main (main) where
import Cpt.LexerParser (startLexer)
import LibParser.Parser (Parser(..))
import System.Exit ( exitSuccess )
import Ast ( Ast (..), cptToAst )
import Cpt.Cpt (Cpt)
import Data.Map (empty)
import Evaluation (Bindings, evalAst)


treatCptList :: [Cpt] -> Bindings -> Maybe Ast
treatCptList [] _ = Nothing
treatCptList (c:[]) bs = cptToAst c >>= \ast -> (\(a, _) -> a) $ evalAst ast bs
treatCptList (c:cs) bs = cptToAst c >>= \ast -> (\(a, newBs) ->
  a >>= (\_ -> treatCptList cs newBs)) $ evalAst ast bs

interpreteInput :: String -> String
interpreteInput str = case runParser startLexer str of
  Left err -> show err
  Right (cpt, _) -> case (treatCptList cpt empty) of
    Just (Value v) -> show v
    Just (Lambda n _) -> "#<procedure " ++ show n ++ ">"
    _ -> "Nothing"

prompt :: String
prompt = "> "

launchCmd :: String -> IO ()
launchCmd "quit" = exitSuccess
launchCmd str = putStr prompt >> putStrLn (interpreteInput str)

loop :: IO ()
loop = getLine >>= \line -> launchCmd line >> loop

main :: IO ()
main = loop
