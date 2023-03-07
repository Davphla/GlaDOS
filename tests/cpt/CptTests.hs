module CptTests (cptTestList) where

import Test.HUnit

import Cpt.Cpt (Cpt (Literal, Identifier, Operation), getIdentifier, getLiteral, getList)
import Cpt.Literal (Literal (Int, Float, Bool))
import Error (GladosError (Cpt), CptError (..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptTestList :: Test
cptTestList = TestList [ getIdentifierIdentifier, getIdentifierInt,
    getIdentifierList, getLiteralInt, getLiteralFloat, getLiteralBool,
    getLiteralIdentifier, getLiteralList, getListList, getListInt,
    getListIdentifier
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
    (Left [Cpt InvalidCpt])
    (getIdentifier (Literal (Int 1)))
    )

getIdentifierList :: Test
getIdentifierList = TestCase (assertEqual "For getIdentifier list"
    (Left [Cpt InvalidCpt])
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
    (Left [Cpt InvalidCpt])
    (getLiteral (Identifier "s"))
    )

getLiteralList :: Test
getLiteralList = TestCase (assertEqual "For getLiteral list"
    (Left [Cpt InvalidCpt])
    (getLiteral (Operation [Identifier "s"]))
    )

-- -------------------------------------------------------------------------- --
--                                getList Tests                               --
-- -------------------------------------------------------------------------- --

getListList :: Test
getListList = TestCase (assertEqual "For getList [\"s\"]"
    (Right [Identifier "s"])
    (getList (Operation [Identifier "s"]))
    )

getListInt :: Test
getListInt = TestCase (assertEqual "For getList 1"
    (Left [Cpt InvalidCpt])
    (getList (Literal (Int 1)))
    )

getListIdentifier :: Test
getListIdentifier = TestCase (assertEqual "For getList Identifier"
    (Left [Cpt InvalidCpt])
    (getList (Identifier "a"))
    )
