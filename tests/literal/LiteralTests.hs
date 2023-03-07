module LiteralTests (literalTestList) where

import Test.HUnit

import Cpt.Literal (Literal (..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

literalTestList :: Test
literalTestList = TestList [
    testFloat, testShowTrue, testShowFalse,
    testShowInt
  ]

-- -------------------------------------------------------------------------- --

testFloat :: Test
testFloat = TestCase (assertEqual "Simple Float point"
    "3.14"
    (show $ Float 3.14)
  )

testShowTrue :: Test
testShowTrue = TestCase (assertEqual "Show true"
    "#t"
    (show $ Bool True)
  )

testShowFalse :: Test
testShowFalse = TestCase (assertEqual "Show false"
    "#f"
    (show $ Bool False)
  )

testShowInt :: Test
testShowInt = TestCase (assertEqual "Show Int"
    "1"
    (show $ Int 1)
  )