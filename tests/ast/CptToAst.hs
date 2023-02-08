{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Operators.hs
-}

module CptToAst (cptToAstTestList) where

import Test.HUnit

import Ast (
    Ast (Operator, Value, Call, Define, Function),
    Operator (Plus, Minus, Times, Div),
    cptToAst,
  )
import Cpt (
    Cpt (Literal, Symbol, List),
  )
import Literal (Literal (Integer, Inexact, Floating, Boolean))


-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptToAstTestList :: Test
cptToAstTestList = TestList [
    simpleAdd, simpleMinus, simpleDivide, simpleTimes, simpleSymbol, simpleInteger, 
    simpleInexact, simpleFloating, simpleBoolean, simpleDefine, defineFunction, defineNothing,
    defineLambda, callTest
  ]

-- -------------------------------------------------------------------------- --
--                                Simple tests                                --
-- -------------------------------------------------------------------------- --

simpleAdd :: Test
simpleAdd = TestCase (assertEqual "For Cpt < + 4 5 >"
    (Just (Operator Plus [Value (Integer 4), Value (Integer 5)]))
    (cptToAst (List [Symbol "+", Literal (Integer 4), Literal (Integer 5)]))
  )

simpleMinus :: Test
simpleMinus = TestCase (assertEqual "For Cpt < - 4 5 >"
    (Just (Operator Minus [Value (Integer 4), Value (Integer 5)]))
    (cptToAst (List [Symbol "-", Literal (Integer 4), Literal (Integer 5)]))
  )

simpleTimes :: Test
simpleTimes = TestCase (assertEqual "For Cpt < * 4 5 >"
    (Just (Operator Times [Value (Integer 4), Value (Integer 5)]))
    (cptToAst (List [Symbol "*", Literal (Integer 4), Literal (Integer 5)]))
  )

simpleDivide :: Test
simpleDivide = TestCase (assertEqual "For Cpt < / 4 5 >"
    (Just (Operator Div [Value (Integer 4), Value (Integer 5)]))
    (cptToAst (List [Symbol "/", Literal (Integer 4), Literal (Integer 5)]))
  )

simpleSymbol :: Test
simpleSymbol = TestCase (assertEqual "For Cpt Symbol x"
    (Just (Call "x" []))
    (cptToAst (Symbol "x"))
  )

simpleInteger :: Test
simpleInteger = TestCase (assertEqual "For Cpt Literal Integer 5"
    (Just (Value (Integer 5)))
    (cptToAst (Literal (Integer 5)))
  )

simpleInexact :: Test
simpleInexact = TestCase (assertEqual "For Cpt Literal Inexact 5 4"
    (Just (Value (Inexact 5 4)))
    (cptToAst (Literal (Inexact 5 4)))
  )

simpleFloating :: Test
simpleFloating = TestCase (assertEqual "For Cpt Literal Floating 5.4"
    (Just (Value (Floating 5.4)))
    (cptToAst (Literal (Floating 5.4)))
  )

simpleBoolean :: Test
simpleBoolean = TestCase (assertEqual "For Cpt Literal Boolean True"
    (Just (Value (Boolean True)))
    (cptToAst (Literal (Boolean True)))
  )

simpleDefine :: Test
simpleDefine = TestCase (assertEqual "For Cpt define x 5"
    (Just (Define "x" (Value (Integer 4))))
    (cptToAst (List [Symbol "define", Symbol "x", Literal (Integer 4)]))  
  )

-- -------------------------------------------------------------------------- --
--                                Define tests                                --
-- -------------------------------------------------------------------------- --

defineFunction :: Test
defineFunction = TestCase (assertEqual "For Cpt define x 5"
    (Just (Define "f" (Function ["a", "b"] (Operator Plus [Call "a" [], Call "b" []]))))
    (cptToAst (List [Symbol "define", List [Symbol "f", Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]))  
  )

defineNothing :: Test
defineNothing = TestCase (assertEqual "For Define Nothing"
    Nothing
    (cptToAst (List [Symbol "define"]))  
  )

defineLambda :: Test
defineLambda = TestCase (assertEqual "For define lambda + a b"
    (Just (Define "f" (Function ["a", "b"] (Operator Plus [Call "a" [], Call "b" []]))))
    (cptToAst (List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]))
  )

-- -------------------------------------------------------------------------- --
--                                 Call tests                                 --
-- -------------------------------------------------------------------------- --

callTest :: Test
callTest = TestCase (assertEqual "For call list [f a]"
    (Just (Call "f" [Call "a" []]))
    (cptToAst (List [Symbol "f", Symbol "a"]))
  )