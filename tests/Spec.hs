{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Spec.hs
-}

import Test.HUnit

import System.Exit (exitWith, exitSuccess, ExitCode (ExitFailure))

import CptToAst (cptToAstTestList)
import CptTests (cptTestList)
import LexerTests (lexerTestList)
import LiteralTests (literalTestList)
import CptOperator (cptOperatorTestList)
import CptKeyWord (cptKeywordTestList)
import ShuntingYardTests (shuntingYardTestList)


main :: IO ()
main = runTestTT ( test [
    cptToAstTestList, cptTestList,
    literalTestList, lexerTestList, cptOperatorTestList, 
    cptKeywordTestList, shuntingYardTestList
  ]) >>= (\x -> if errors x + failures x == 0
    then  exitSuccess
    else exitWith (ExitFailure 84))
