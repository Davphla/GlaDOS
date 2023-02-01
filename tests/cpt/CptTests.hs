module CptTests (cptTestList) where

import Test.HUnit

import Cpt (Cpt (Literal, Symbol, List), getSymbol, getLiteral, getList, printTree)
import Literal (Literal (Integer, Inexact, Floating, Boolean))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptTestList :: Test
cptTestList = TestList [ getSymbolSymbol, getSymbolInt, getSymbolList, getLiteralInteger, getLiteralInexact, 
    getLiteralFloat, getLiteralBool, getLiteralSymbol, getLiteralList, getListList, getListInt, getListSymbol,
    printTreeInteger, printTreeInexact, printTreeFloating, printTreeBooleanTrue, printTreeBooleanFalse, printTreeSymbol]

-- -------------------------------------------------------------------------- --
--                               getSymbol Tests                              --
-- -------------------------------------------------------------------------- --

getSymbolSymbol :: Test
getSymbolSymbol = TestCase (assertEqual "For getSymbol \"s\""
    (Just "s")
    (getSymbol (Symbol "s"))
    )

getSymbolInt :: Test
getSymbolInt = TestCase (assertEqual "For getSymbol Literal Int"
    Nothing
    (getSymbol (Literal (Integer 1)))
    )

getSymbolList :: Test
getSymbolList = TestCase (assertEqual "For getSymbol list"
    Nothing
    (getSymbol (List [Symbol "s"]))
    )

-- -------------------------------------------------------------------------- --
--                              getLiteral Tests                              --
-- -------------------------------------------------------------------------- --

getLiteralInteger :: Test
getLiteralInteger = TestCase (assertEqual "For getLiteral Int"
    (Just (Integer 1))
    (getLiteral (Literal (Integer 1)))
    )

getLiteralInexact :: Test
getLiteralInexact = TestCase (assertEqual "For getLiteral Inexact"
    (Just (Inexact 1 2))
    (getLiteral (Literal (Inexact 1 2)))
    )

getLiteralFloat :: Test
getLiteralFloat = TestCase (assertEqual "For getLiteral float"
    (Just (Floating 1.42))
    (getLiteral (Literal (Floating 1.42)))
    )

getLiteralBool :: Test
getLiteralBool = TestCase (assertEqual "For getLiteral bool"
    (Just (Boolean True))
    (getLiteral (Literal (Boolean True)))
    )

getLiteralSymbol :: Test
getLiteralSymbol = TestCase (assertEqual "For getLiteral symbol"
    Nothing
    (getLiteral (Symbol "s"))
    )

getLiteralList :: Test
getLiteralList = TestCase (assertEqual "For getLiteral list"
    Nothing
    (getLiteral (List [Symbol "s"]))
    )

-- -------------------------------------------------------------------------- --
--                                getList Tests                               --
-- -------------------------------------------------------------------------- --

getListList :: Test
getListList = TestCase (assertEqual "For getList [\"s\"]"
    (Just [Symbol "s"])
    (getList (List [Symbol "s"]))
    )

getListInt :: Test
getListInt = TestCase (assertEqual "For getList 1"
    Nothing
    (getList (Literal (Integer 1)))
    )

getListSymbol :: Test
getListSymbol = TestCase (assertEqual "For getList symbol"
    Nothing
    (getList (Symbol "a"))
    )

-- -------------------------------------------------------------------------- --
--                               printTree Tests                              --
-- -------------------------------------------------------------------------- --

printTreeInteger :: Test
printTreeInteger = TestCase (assertEqual "For printTree Integer"
    (Just "a Number 42")
    (printTree (Literal (Integer 42)))
    )

printTreeInexact :: Test
printTreeInexact = TestCase (assertEqual "For printTree Integer"
    (Just "a Number 42/69")
    (printTree (Literal (Inexact 42 69)))
    )

printTreeFloating :: Test
printTreeFloating = TestCase (assertEqual "For printTree Integer"
    (Just "a Number 42.69")
    (printTree (Literal (Floating 42.69)))
    )

printTreeBooleanTrue :: Test
printTreeBooleanTrue = TestCase (assertEqual "For printTree Integer"
    (Just "a Number #t")
    (printTree (Literal (Boolean True)))
    )

printTreeBooleanFalse :: Test
printTreeBooleanFalse = TestCase (assertEqual "For printTree Integer"
    (Just "a Number #f")
    (printTree (Literal (Boolean False)))
    )

printTreeSymbol :: Test
printTreeSymbol = TestCase (assertEqual "For printTree Integer"
    (Just "a Symbol 's'")
    (printTree (Symbol "s"))
    )
