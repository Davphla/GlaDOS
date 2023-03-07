{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Operators.hs
-}

module CptToAst (cptToAstTestList) where

import Test.HUnit

import Ast.Ast (
    Ast (Operation, Value, Call, Define, Lambda),
    cptToAst,
  )
import Cpt.Cpt (
    Cpt (Literal, Identifier, Operation),
  )
import Cpt.Literal (Literal (Int, Float, Bool))
import Cpt.Operator (Operator (..), OperatorType (Plus, Minus, Times, Div))
import Error (GladosError (Ast))

import GHC.Generics (Associativity (LeftAssociative))


-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptToAstTestList :: Test
cptToAstTestList = TestList [
    simpleAdd, simpleMinus, simpleDivide, simpleTimes, simpleIdentifier, simpleInt,
    simpleFloat, simpleBool, simpleDefine, defineFunction,
    defineLambda, callTest
  ]

-- -------------------------------------------------------------------------- --
--                                Simple tests                                --
-- -------------------------------------------------------------------------- --

simpleAdd :: Test
simpleAdd = TestCase (assertEqual "For Cpt < + 4 5 >"
    (Right (Ast.Ast.Operation (Cpt.Operator.Operator Plus 10 LeftAssociative) [Value (Int 4), Value (Int 5)]))
    (cptToAst (Cpt.Cpt.Operation [Identifier "+", Literal (Int 4), Literal (Int 5)]))
  )

simpleMinus :: Test
simpleMinus = TestCase (assertEqual "For Cpt < - 4 5 >"
    (Right (Ast.Ast.Operation (Cpt.Operator.Operator Minus 10 LeftAssociative) [Value (Int 4), Value (Int 5)]))
    (cptToAst (Cpt.Cpt.Operation [Identifier "-", Literal (Int 4), Literal (Int 5)]))
  )

simpleTimes :: Test
simpleTimes = TestCase (assertEqual "For Cpt < * 4 5 >"
    (Right (Ast.Ast.Operation (Cpt.Operator.Operator Times 20 LeftAssociative) [Value (Int 4), Value (Int 5)]))
    (cptToAst (Cpt.Cpt.Operation [Identifier "*", Literal (Int 4), Literal (Int 5)]))
  )

simpleDivide :: Test
simpleDivide = TestCase (assertEqual "For Cpt < / 4 5 >"
    (Right (Ast.Ast.Operation (Cpt.Operator.Operator Div 20 LeftAssociative) [Value (Int 4), Value (Int 5)]))
    (cptToAst (Cpt.Cpt.Operation [Identifier "/", Literal (Int 4), Literal (Int 5)]))
  )

simpleIdentifier :: Test
simpleIdentifier = TestCase (assertEqual "For Cpt Identifier x"
    (Right (Call "x" []))
    (cptToAst (Identifier "x"))
  )

simpleInt :: Test
simpleInt = TestCase (assertEqual "For Cpt Literal Int 5"
    (Right (Value (Int 5)))
    (cptToAst (Literal (Int 5)))
  )

simpleFloat :: Test
simpleFloat = TestCase (assertEqual "For Cpt Literal Float 5.4"
    (Right (Value (Float 5.4)))
    (cptToAst (Literal (Float 5.4)))
  )

simpleBool :: Test
simpleBool = TestCase (assertEqual "For Cpt Literal Bool True"
    (Right (Value (Bool True)))
    (cptToAst (Literal (Bool True)))
  )

simpleDefine :: Test
simpleDefine = TestCase (assertEqual "For Cpt define x 5"
    (Right (Define "x" (Value (Int 4))))
    (cptToAst (Cpt.Cpt.Operation [Identifier "define", Identifier "x", Literal (Int 4)]))
  )

-- -------------------------------------------------------------------------- --
--                                Define tests                                --
-- -------------------------------------------------------------------------- --

defineFunction :: Test
defineFunction = TestCase (assertEqual "For Cpt define x 5"
    (Right (Define "f" (Lambda ["a", "b"] (Ast.Ast.Operation (Cpt.Operator.Operator Plus 10 LeftAssociative) [Call "a" [], Call "b" []]))))
    (cptToAst (Cpt.Cpt.Operation [Identifier "define", Cpt.Cpt.Operation [Identifier "f", Identifier "a", Identifier "b"], Cpt.Cpt.Operation [Identifier "+", Identifier "a", Identifier "b"]]))
  )

defineLambda :: Test
defineLambda = TestCase (assertEqual "For define lambda + a b"
    (Right (Define "f" (Lambda ["a", "b"] (Ast.Ast.Operation (Cpt.Operator.Operator Plus 10 LeftAssociative) [Call "a" [], Call "b" []]))))
    (cptToAst (Cpt.Cpt.Operation [Identifier "lambda", Cpt.Cpt.Operation [Identifier "a", Identifier "b"], Cpt.Cpt.Operation [Identifier "+", Identifier "a", Identifier "b"]]))
  )

-- -------------------------------------------------------------------------- --
--                                 Call tests                                 --
-- -------------------------------------------------------------------------- --

callTest :: Test
callTest = TestCase (assertEqual "For call list [f a]"
    (Right (Call "f" [Call "a" []]))
    (cptToAst (Cpt.Cpt.Operation [Identifier "f", Identifier "a"]))
  )