module CptOperator (cptOperatorTestList) where

import Test.HUnit

import Cpt.Operator
import GHC.Generics (Associativity (..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptOperatorTestList :: Test
cptOperatorTestList = TestList [operatorFromStrPlus, operatorFromStrMinus, operatorFromStrTimes, operatorFromStrDiv, operatorFromStrMod, operatorFromStrNothing, operatorsString]

-- -------------------------------------------------------------------------- --
--                               operatorFromStr                              --
-- -------------------------------------------------------------------------- --

operatorFromStrPlus :: Test
operatorFromStrPlus = TestCase (assertEqual "For operatorFromStr on +"
    (Just $ Operator Plus 10 LeftAssociative)
    (operatorFromStr "+")
    )

operatorFromStrMinus :: Test
operatorFromStrMinus = TestCase (assertEqual "For operatorFromStr on -"
    (Just $ Operator Minus 10 LeftAssociative)
    (operatorFromStr "-")
    )

operatorFromStrTimes :: Test
operatorFromStrTimes = TestCase (assertEqual "For operatorFromStr on *"
    (Just $ Operator Times 20 LeftAssociative)
    (operatorFromStr "*")
    )

operatorFromStrDiv :: Test
operatorFromStrDiv = TestCase (assertEqual "For operatorFromStr on /"
    (Just $ Operator Div 20 LeftAssociative)
    (operatorFromStr "/")
    )

operatorFromStrMod :: Test
operatorFromStrMod = TestCase (assertEqual "For operatorFromStr on %"
    (Just $ Operator Mod 20 LeftAssociative)
    (operatorFromStr "%")
    )

operatorFromStrNothing :: Test
operatorFromStrNothing = TestCase (assertEqual "For operatorFromStr for nothin"
    (Nothing)
    (operatorFromStr "a")
    )

-- -------------------------------------------------------------------------- --
--                                  Operators                                 --
-- -------------------------------------------------------------------------- --

operatorsString :: Test
operatorsString = TestCase (assertEqual "For operators"
    ([".", "+", "-", "*", "/", "`function`", "::", "->", "$"])
    (operators)
    )
