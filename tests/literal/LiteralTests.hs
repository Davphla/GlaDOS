module LiteralTests (literalTestList) where

import Test.HUnit

import Literal (Literal (..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

literalTestList :: Test
literalTestList = TestList [
    testFloating, testShowTrue, testShowFalse,
    testShowInteger, testShowInexact
  ]

-- -------------------------------------------------------------------------- --

testFloating :: Test
testFloating = TestCase (assertEqual "Simple floating point"
    "3.14"
    (show $ Floating 3.14)
  )

testShowTrue :: Test
testShowTrue = TestCase (assertEqual "Show true"
    "#t"
    (show $ Boolean True)
  )

testShowFalse :: Test
testShowFalse = TestCase (assertEqual "Show false"
    "#f"
    (show $ Boolean False)
  )

testShowInexact :: Test
testShowInexact = TestCase (assertEqual "Show inexact"
    "1/3"
    (show $ Inexact 1 3)
  )

testShowInteger :: Test
testShowInteger = TestCase (assertEqual "Show integer"
    "1"
    (show $ Integer 1)
  )