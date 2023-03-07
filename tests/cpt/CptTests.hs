module CptTests (cptTestList) where

import Test.HUnit

import Cpt.Cpt (Cpt (Literal, Identifier, Operation), getIdentifier, getLiteral, getExpression)
import Cpt.Literal (Literal (Int, Float, Bool))
import Error (GladosError (Cpt), CptError (..), CptErrorReason (..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptTestList :: Test
cptTestList = TestList [ getIdentifierIdentifier, getIdentifierInt,
    getIdentifierList, getLiteralInt, getLiteralFloat, getLiteralBool,
    getLiteralIdentifier, getLiteralList, getExpressionList, getExpressionInt,
    getExpressionIdentifier
    ]
-- -------------------------------------------------------------------------- --
--                               getIdentifier Tests                              --
-- -------------------------------------------------------------------------- --

getIdentifierIdentifier :: Test
getIdentifierIdentifier = TestCase (assertEqual "For getIdentifier \"s\""
    (Right "s")
    (getIdentifier (Identifier "s"))
    )

getIdentifierInt :: Test
getIdentifierInt = TestCase (assertEqual "For getIdentifier Literal Int"
    (Left [Cpt $ InvalidCpt InvalidCptNotIdentifier $ show (Literal (Int 1))])
    (getIdentifier (Literal (Int 1)))
    )

getIdentifierList :: Test
getIdentifierList = TestCase (assertEqual "For getIdentifier list"
    (Left [Cpt $ InvalidCpt InvalidCptNotIdentifier $ show (Operation [Identifier "s"])])
    (getIdentifier (Operation [Identifier "s"]))
    )

-- -------------------------------------------------------------------------- --
--                              getLiteral Tests                              --
-- -------------------------------------------------------------------------- --

getLiteralInt :: Test
getLiteralInt = TestCase (assertEqual "For getLiteral Int"
    (Right (Int 1))
    (getLiteral (Literal (Int 1)))
    )

getLiteralFloat :: Test
getLiteralFloat = TestCase (assertEqual "For getLiteral float"
    (Right (Float 1.42))
    (getLiteral (Literal (Float 1.42)))
    )

getLiteralBool :: Test
getLiteralBool = TestCase (assertEqual "For getLiteral bool"
    (Right (Bool True))
    (getLiteral (Literal (Bool True)))
    )

getLiteralIdentifier :: Test
getLiteralIdentifier = TestCase (assertEqual "For getLiteral Identifier"
    (Left [Cpt $ InvalidCpt InvalidCptNotLiteral $ show (Identifier "s")])
    (getLiteral (Identifier "s"))
    )

getLiteralList :: Test
getLiteralList = TestCase (assertEqual "For getLiteral list"
    (Left [Cpt $ InvalidCpt InvalidCptNotLiteral $ show (Operation [Identifier "s"])])
    (getLiteral (Operation [Identifier "s"]))
    )

-- -------------------------------------------------------------------------- --
--                                getExpression Tests                               --
-- -------------------------------------------------------------------------- --

getExpressionList :: Test
getExpressionList = TestCase (assertEqual "For getExpression [\"s\"]"
    (Right [Identifier "s"])
    (getExpression (Operation [Identifier "s"]))
    )

getExpressionInt :: Test
getExpressionInt = TestCase (assertEqual "For getExpression 1"
    (Left [Cpt $ InvalidCpt InvalidCptNotExpression $ show (Literal (Int 1))])
    (getExpression (Literal (Int 1)))
    )

getExpressionIdentifier :: Test
getExpressionIdentifier = TestCase (assertEqual "For getExpression Identifier"
    (Left [Cpt $ InvalidCpt InvalidCptNotExpression $ show (Identifier "a")])
    (getExpression (Identifier "a"))
    )
