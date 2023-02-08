{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Operators.hs
-}

module EvaluationOperatorTests (operatorTestList) where

import Test.HUnit
import Prelude hiding (lookup)


import Ast (Ast (..), Operator (..))
import Data.Map (empty)
import Evaluation (evalAst)
import Literal (Literal (..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

operatorTestList :: Test
operatorTestList = TestList [
    simplePlusTest, onePlusTest, noPlusTest, multiPlusTest,
    simpleMinusTest, oneMinusTest, noMinusTest, multiMinusTest,
    simpleMultiplyTest, oneMultiplyTest, noMultiplyTest, multiMultiplyTest,
    simpleDivideTest, oneDivideTest, noDivideTest, multiDivideTest
  ]

-- -------------------------------------------------------------------------- --
--                               Operator tests                               --
-- -------------------------------------------------------------------------- --

simplePlusTest :: Test
simplePlusTest = TestCase (assertEqual "For a simple plus"
    (Just (Value (Integer 7)), empty)
    (evalAst (Operator Plus [Value (Integer 3), Value (Integer 4)]) empty)
  )

onePlusTest :: Test
onePlusTest = TestCase (assertEqual "For a plus with one argument"
    (Just (Value (Integer 3)))
    (fst $ evalAst (Operator Plus [Value (Integer 3)]) empty)
  )

multiPlusTest :: Test
multiPlusTest = TestCase (assertEqual "For a plus with multiple arguments"
    (Just (Value (Integer 10)))
    (fst $ evalAst (Operator Plus [Value (Integer 3), Value (Integer 4), Value (Integer 3)]) empty)
  )

noPlusTest :: Test
noPlusTest = TestCase (assertEqual "For a plus with no argument"
    (Just (Value (Integer 0)))
    (fst $ evalAst (Operator Plus []) empty)
  )

simpleMinusTest :: Test
simpleMinusTest = TestCase (assertEqual "For a simple minus"
    (Just (Value (Integer 1)))
    (fst $ evalAst (Operator Minus [Value (Integer 4), Value (Integer 3)]) empty)
  )

oneMinusTest :: Test
oneMinusTest = TestCase (assertEqual "For a minus with one argument"
    (Just (Value (Integer (-3))))
    (fst $ evalAst (Operator Minus [Value (Integer 3)]) empty)
  )

multiMinusTest :: Test
multiMinusTest = TestCase (assertEqual "For a minus with multiple argument"
    (Just (Value (Integer 0)))
    (fst $ evalAst (Operator Minus [Value (Integer 10), Value (Integer 4), Value (Integer 3), Value (Integer 3)]) empty)
  )

noMinusTest :: Test
noMinusTest = TestCase (assertEqual "For a minus with no argument"
    Nothing
    (fst $ evalAst (Operator Minus []) empty)
  )

simpleMultiplyTest :: Test
simpleMultiplyTest = TestCase (assertEqual "For a simple multiply"
    (Just (Value (Integer 12)))
    (fst $ evalAst (Operator Times [Value (Integer 4), Value (Integer 3)]) empty)
  )

oneMultiplyTest :: Test
oneMultiplyTest = TestCase (assertEqual "For a multiply with one argument"
    (Just (Value (Integer 3)))
    (fst $ evalAst (Operator Times [Value (Integer 3)]) empty)
  )

noMultiplyTest :: Test
noMultiplyTest = TestCase (assertEqual "For a simple multiply"
    (Just (Value (Integer 1)))
    (fst $ evalAst (Operator Times []) empty)
  )

multiMultiplyTest :: Test
multiMultiplyTest = TestCase (assertEqual "For a multiply with multiple parameters"
    (Just (Value (Integer 24)))
    (fst $ evalAst (Operator Times [Value (Integer 4), Value (Integer 3), Value (Integer 2)]) empty)
  )

simpleDivideTest :: Test
simpleDivideTest = TestCase (assertEqual "For a simple divide"
    (Just (Value (Integer 2)))
    (fst $ evalAst (Operator Div [Value (Integer 4), Value (Integer 2)]) empty)
  )

oneDivideTest :: Test
oneDivideTest = TestCase (assertEqual "For a divide with one argument"
    (Just (Value (Integer 0))) -- Inexact 1 3 when handled
    (fst $ evalAst (Operator Div [Value (Integer 3)]) empty)
  )

noDivideTest :: Test
noDivideTest = TestCase (assertEqual "For a divide with no argument"
    Nothing
    (fst $ evalAst (Operator Div []) empty)
  )

multiDivideTest :: Test
multiDivideTest = TestCase (assertEqual "For a divide with multiple parameters"
    (Just (Value (Integer 5)))
    (fst $ evalAst (Operator Div [Value (Integer 20), Value (Integer 2), Value (Integer 2)]) empty)
  )