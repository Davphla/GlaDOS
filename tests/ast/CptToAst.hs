{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Operators.hs
-}

module CptToAst (cptToAstTestList) where

import Test.HUnit

import Ast.Ast (
    Ast (Condition, Operation, Value, Call, Define, Lambda),
    cptToAst,
    expressionToAst,
  )
import Cpt.Cpt (
    Cpt (Expression, Lambda, Literal, Identifier, Operation, Prototype),
    Condition,
    Prototype
  )
import Cpt.Literal (Literal (Int, Float, Bool))
import Cpt.Operator (Operator (..), OperatorType (Plus, Minus, Times, Div))
import Cpt.LexerParser (pCpt, pExpression)
import Error (
  GlobalWarning (..),
  CptError (..),
  GladosError (..),
  CptErrorReason (InvalidCptNotTreatable)
  )
import LibParser.Parser (runParser)
import GHC.Generics (Associativity (LeftAssociative))


-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptToAstTestList :: Test
cptToAstTestList = TestList [
    simpleAssignment,
    wrongCptType, prototypeCpt,
    defineFunction, defineLambda, lambdaCreation
  ]

-- -------------------------------------------------------------------------- --
--                                Simple tests                                --
-- -------------------------------------------------------------------------- --

simpleAssignment :: Test
simpleAssignment = TestCase (assertEqual "For assignment toto = 1 + 1"
    (Right (Ast.Ast.Define "toto" (Ast.Ast.Operation (Cpt.Operator.Operator Plus 10 LeftAssociative) [Ast.Ast.Value (Int 1), Ast.Ast.Value (Int 1)])))
    (runParser pCpt "toto = 1 + 1" >>= (cptToAst . fst))
  )

-- conditionalAssignment :: Test
-- conditionalAssignment = TestCase (assertEqual "For assignment toto = if 1 then 2 else 3"
    -- (Right (Ast.Ast.Define "toto" (Ast.Ast.Condition (Ast.Ast.Value (Int 1)) (Ast.Ast.Value (Int 2)) (Ast.Ast.Value (Int 3)))))
    -- (runParser pCpt "toto = if 1 then 2 else 3" >>= (cptToAst . fst))
  -- )

wrongCptType :: Test
wrongCptType = TestCase (assertEqual "For a cpt not assignment or prototype"
    (Left [Cpt $ InvalidCpt InvalidCptNotTreatable $ show (Cpt.Cpt.Literal (Int 1))])
    (cptToAst (Cpt.Cpt.Literal (Int 1)))
  )

prototypeCpt :: Test
prototypeCpt = TestCase (assertEqual "Prototype in cptToAst"
    (Left [Warning $ NotImplemented "function prototypes"])
    (cptToAst (Cpt.Cpt.Prototype ("Marvin", ["Le", "BG"])))
  )

-- -------------------------------------------------------------------------- --
--                                Define tests                                --
-- -------------------------------------------------------------------------- --

defineFunction :: Test
defineFunction = TestCase (assertEqual "For toto a = a + 5"
    (Right (Define "toto" (Ast.Ast.Lambda ["a"] (Ast.Ast.Operation (Cpt.Operator.Operator Plus 10 LeftAssociative) [Value (Int 1), Value (Int 5)]))))
    (runParser pCpt "toto a = 1 + 5" >>= (cptToAst . fst))
  )

defineLambda :: Test
defineLambda = TestCase (assertEqual "For define lambda + a b"
    (Left [Cpt $ InvalidCpt InvalidCptNotTreatable $ show (Cpt.Cpt.Operation [Identifier "lambda", Cpt.Cpt.Operation [Identifier "a", Identifier "b"], Cpt.Cpt.Operation [Identifier "+", Identifier "a", Identifier "b"]])])
    (cptToAst (Cpt.Cpt.Operation [Identifier "lambda", Cpt.Cpt.Operation [Identifier "a", Identifier "b"], Cpt.Cpt.Operation [Identifier "+", Identifier "a", Identifier "b"]]))
  )

lambdaCreation :: Test
lambdaCreation = TestCase (assertEqual "Simple lambda test"
    (Left [Warning $ NotImplemented "lambdas"])
    (expressionToAst [
      Cpt.Cpt.Lambda [
        Cpt.Cpt.Identifier "a", Cpt.Cpt.Identifier "b", 
        (Cpt.Cpt.Operation [
          Cpt.Cpt.Identifier "+", Cpt.Cpt.Identifier "a", Cpt.Cpt.Identifier "b"]
        )
    ]])
  )
