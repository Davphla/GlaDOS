module Operators (operatorTestList) where

import Test.HUnit

import Ast (
  Ast (Operator, Value),
  Operator (Plus, Minus, Times, Div, Mod),
  cptToAst,
  ValueType (Number))
import Cpt (Cpt (Integer, Symbol, List))


-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

operatorTestList :: Test
operatorTestList = TestList [
    simpleAdd, simpleDivide, simpleTimes,
    simpleModulo, errorMinus, errorDivide,
    errorModuloNoArguments, errorModuloOneArgument, errorModuloTooMuchArguments
  ]

-- -------------------------------------------------------------------------- --
--                                Simple tests                                --
-- -------------------------------------------------------------------------- --

simpleAdd :: Test
simpleAdd = TestCase (assertEqual "For Cpt < + 4 5 >"
    (cptToAst (List [Symbol "+", Integer 4, Integer 5]))
    (Just (Operator Plus [Value (Number 4), Value (Number 5)]))
  )

simpleMinus :: Test
simpleMinus = TestCase (assertEqual "For Cpt < - 4 5 >"
    (cptToAst (List [Symbol "+", Integer 4, Integer 5]))
    (Just (Operator Minus [Value (Number 4), Value (Number 5)]))
  )

simpleTimes :: Test
simpleTimes = TestCase (assertEqual "For Cpt < * 4 5 >"
    (cptToAst (List [Symbol "+", Integer 4, Integer 5]))
    (Just (Operator Times [Value (Number 4), Value (Number 5)]))
  )

simpleDivide :: Test
simpleDivide = TestCase (assertEqual "For Cpt < / 4 5 >"
    (cptToAst (List [Symbol "+", Integer 4, Integer 5]))
    (Just (Operator Div [Value (Number 4), Value (Number 5)]))
  )

simpleModulo :: Test
simpleModulo = TestCase (assertEqual "For Cpt < mod 4 5 >"
    (cptToAst (List [Symbol "+", Integer 4, Integer 5]))
    (Just (Operator Mod [Value (Number 4), Value (Number 5)]))
  )

-- -------------------------------------------------------------------------- --
--                                Error tests                                 --
-- -------------------------------------------------------------------------- --

errorMinus :: Test
errorMinus = TestCase (assertEqual "For Cpt < - >"
    (cptToAst (List [Symbol "-"]))
    Nothing
  )

errorDivide :: Test
errorDivide = TestCase (assertEqual "For Cpt < / >"
    (cptToAst (List [Symbol "/"]))
    Nothing
  )

errorModuloNoArguments :: Test
errorModuloNoArguments = TestCase (assertEqual "For Cpt < mod >"
    (cptToAst (List [Symbol "/", Integer 4, Integer 0]))
    (Nothing)
  )

errorModuloOneArgument :: Test
errorModuloOneArgument = TestCase (assertEqual "For Cpt < mod 3 >"
    (cptToAst (List [Symbol "mod", Integer 3]))
    (Nothing)
  )

errorModuloTooMuchArguments :: Test
errorModuloTooMuchArguments = TestCase (assertEqual "For Cpt < mod 3 4 5 6 >"
    (cptToAst (List [Symbol "mod", Integer 3, Integer 4, Integer 5, Integer 6]))
    (Nothing)
  )
