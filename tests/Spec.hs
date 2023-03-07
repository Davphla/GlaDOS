{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Spec.hs
-}

import Test.HUnit

import System.Exit (exitWith, exitSuccess, ExitCode (ExitFailure))

import AstTests (astTestList)
import CptToAst (cptToAstTestList)
import CptTests (cptTestList)
import LexerTests (lexerTestList)
import LiteralTests (literalTestList)


main :: IO ()
main = runTestTT ( test [
    cptToAstTestList, cptTestList, astTestList, evaluationTestList,
    operatorTestList, literalTestList, lexerTestList
  ]) >>= (\x -> if errors x + failures x == 0
    then  exitSuccess
    else exitWith (ExitFailure 84))
