module ShuntingYardTests (shuntingYardTestList) where

import Ast.ShuntingYard (shuntingYard, associativity)
import Cpt.Cpt (Cpt (..))
import Cpt.Operator (Operator (..), OperatorType (Plus, Times))
import Cpt.LexerParser (pExpression)
import Cpt.Literal (Literal (Int))
import LibParser.Parser (runParser)

import GHC.Generics (Associativity (LeftAssociative))
import Test.HUnit

-- -------------------------------------------------------------------------- --
--                                  test list                                 --
-- -------------------------------------------------------------------------- --

shuntingYardTestList :: Test
shuntingYardTestList = TestList [basicShuntingYard, parenthesisShuntingYard, 
    samePrecedenceShuntingYard, functionCallShuntingYard, defaultAssociativityTest]

-- -------------------------------------------------------------------------- --
--                                 basic test                                 --
-- -------------------------------------------------------------------------- --

basicShuntingYard :: Test
basicShuntingYard = TestCase (assertEqual "For a basic shunting yard operation"
    (Right [Cpt.Cpt.Literal (Int 1), Cpt.Cpt.Literal (Int 2), Cpt.Cpt.Literal (Int 3), 
      Cpt.Cpt.Operator (Cpt.Operator.Operator Times 20 LeftAssociative), Cpt.Cpt.Operator (Cpt.Operator.Operator Plus 10 LeftAssociative)])
    (runParser pExpression "1 + 2 * 3" >>= (\(Operation o, _) -> shuntingYard o))
  )

parenthesisShuntingYard :: Test
parenthesisShuntingYard = TestCase (assertEqual "For a shunting yard with parenthesis"
    (Right [Cpt.Cpt.Literal (Int 1), Cpt.Cpt.Literal (Int 2),
      Cpt.Cpt.Operator (Cpt.Operator.Operator Plus 10 LeftAssociative),
      Cpt.Cpt.Literal (Int 3), 
      Cpt.Cpt.Operator (Cpt.Operator.Operator Times 20 LeftAssociative)])
    (runParser pExpression "(1 + 2) * 3" >>= (\(Operation o, _) -> shuntingYard o))
  )

samePrecedenceShuntingYard :: Test
samePrecedenceShuntingYard = TestCase (assertEqual "For an operation with same precedence operators"
    (Right [Cpt.Cpt.Literal (Int 1), Cpt.Cpt.Literal (Int 2),
    Cpt.Cpt.Operator (Cpt.Operator.Operator Plus 10 LeftAssociative),
    Cpt.Cpt.Literal (Int 3), Cpt.Cpt.Operator (Cpt.Operator.Operator Plus 10 LeftAssociative)])
    (runParser pExpression "1 + 2 + 3" >>= (\(Operation o, _) -> shuntingYard o))
  )

functionCallShuntingYard :: Test
functionCallShuntingYard = TestCase (assertEqual "For an operation with function call"
    (Right [Cpt.Cpt.Literal (Int 1), Cpt.Cpt.Literal (Int 2),
    Cpt.Cpt.Operator (Cpt.Operator.Operator Plus 10 LeftAssociative), Cpt.Cpt.Identifier "toto"])
    (runParser pExpression "toto 1 + 2" >>= (\(Operation o, _) -> shuntingYard o))
  )

defaultAssociativityTest :: Test
defaultAssociativityTest = TestCase (assertEqual "Get default associativity"
    (associativity (Cpt.Cpt.Identifier "toto"))
    LeftAssociative
  )