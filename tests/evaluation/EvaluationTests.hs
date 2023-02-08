{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Operators.hs
-}

module EvaluationTests (evaluationTestList) where


import Test.HUnit
import Ast (Ast (..), Operator (..))
import Evaluation (evalAst)
import Literal (Literal (..))
import Data.Map (fromList, empty)


-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

evaluationTestList :: Test
evaluationTestList = TestList [
    callVariableTest, callFunctionTest, callGlobalTest, callOverrideTest,
    callInvalidTest,
    defineFunctionTest, defineLambdaTest, defineVariableTest,
    evalFunctionTest
  ]


-- -------------------------------------------------------------------------- --
--                                EvalAst tests                               --
-- -------------------------------------------------------------------------- --

evalFunctionTest :: Test
evalFunctionTest = TestCase (assertEqual "Simple function without call"
    (Just (Function ["a", "b"] (Operator Plus [Call "a" [], Call "b" []])), empty)
    (evalAst (Function ["a", "b"] (Operator Plus [Call "a" [], Call "b" []])) empty)
  )

-- -------------------------------------------------------------------------- --
--                                  Call Tests                                --
-- -------------------------------------------------------------------------- --

callVariableTest :: Test
callVariableTest = TestCase (assertEqual "For a simple variable call"
    (Just (Value (Integer 4)), fromList [("x", Value (Integer 4))])
    (evalAst (Call "x" []) (fromList [("x", Value (Integer 4))]))
  )

callFunctionTest :: Test
callFunctionTest = TestCase (assertEqual "For a simple function call"
    (Just (Value (Integer 7)))
    (fst $ evalAst (Call "f" [Value (Integer 3), Value (Integer 4)]) (fromList [("f", Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))
    ]))
  )

callGlobalTest :: Test
callGlobalTest = TestCase (assertEqual "For a global variable call in a func"
    (Just (Value (Integer 11)))
    (fst $ evalAst (Call "f" [Value (Integer 3), Value (Integer 4)]) (fromList [
      ("f", Function ["a", "b"]
        (Operator Plus [Call "a" [], Call "b" [], Call "x" []])),
      ("x", Value (Integer 4))
    ]))
  )

callOverrideTest :: Test
callOverrideTest = TestCase (assertEqual "For a parameter that overrides a var"
    (Just (Value (Integer 5)))
    (fst $ evalAst (Call "f" [Value (Integer 3), Value (Integer 2)])
    (fromList [
      ("f", Function ["a", "b"] (Operator Plus [Call "a" [],
        Call "b" []])),
      ("a", Value (Integer 4))
    ]))
  )

callInvalidTest :: Test
callInvalidTest = TestCase (assertEqual "For a call that should not exist"
    Nothing
    (fst $ evalAst (Call "i" []) $ fromList [("i", Operator Plus [Value (Integer 3)])])
  )

-- -------------------------------------------------------------------------- --
--                                Define tests                                --
-- -------------------------------------------------------------------------- --

defineVariableTest :: Test
defineVariableTest = TestCase (assertEqual "For a simple variable definition"
    (Nothing, fromList [("x", Value (Integer 4))])
    (evalAst (Define "x" (Value (Integer 4))) empty)
  )

defineLambdaTest :: Test
defineLambdaTest = TestCase (assertEqual "For a simple function definition"
    (Nothing, fromList [("f", Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))])
    (evalAst (Define "f" (Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))) empty)
  )

defineFunctionTest :: Test
defineFunctionTest = TestCase (assertEqual "For a simple function definition"
    (Nothing, fromList [("f", Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))])
    (evalAst (Define "f" (Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))) empty)
  )
