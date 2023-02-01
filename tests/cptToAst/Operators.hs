{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Operators.hs
-}

module Operators (operatorTestList) where

import Test.HUnit

import Ast (
    Ast (Operator, Value),
    Operator (Plus, Minus, Times, Div),
    cptToAst,
  )
import Cpt (
    Cpt (Literal, Symbol, List),
  )
import Literal (Literal (Integer))


-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

operatorTestList :: Test
operatorTestList = TestList [
    simpleAdd, simpleMinus, simpleDivide, simpleTimes,
    errorMinus, errorDivide
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

-- -------------------------------------------------------------------------- --
--                                Error tests                                 --
-- -------------------------------------------------------------------------- --

errorMinus :: Test
errorMinus = TestCase (assertEqual "For Cpt < - >"
    Nothing
    (cptToAst (List [Symbol "-"]))
  )

errorDivide :: Test
errorDivide = TestCase (assertEqual "For Cpt < / >"
    Nothing
    (cptToAst (List [Symbol "/"]))
  )
