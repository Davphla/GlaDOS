module AstTests (astTestList) where

import Test.HUnit

import Ast (
    listToParams, Params, listToAst
    )
import Cpt (
    Cpt (Literal, Symbol, List),
    )
import Literal (Literal(Integer))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

astTestList :: Test
astTestList = TestList [listToParamsFullSymbols, listToParamsNotFullSymbols, listToAstInteger]

-- -------------------------------------------------------------------------- --
--                             listToParams Tests                             --
-- -------------------------------------------------------------------------- --

listToParamsFullSymbols :: Test
listToParamsFullSymbols = TestCase (assertEqual "For listToParams [a, b]"
    (Just ["a", "b"])
    (listToParams [Symbol "a", Symbol "b"])
    )

listToParamsNotFullSymbols :: Test
listToParamsNotFullSymbols  = TestCase (assertEqual "For listToParams [1, b]"
    Nothing
    (listToParams [Literal (Integer 1), Symbol "b"])
    )

-- -------------------------------------------------------------------------- --
--                               listToAst Tests                              --
-- -------------------------------------------------------------------------- --

listToAstInteger :: Test
listToAstInteger = TestCase (assertEqual "For listToAst Integer"
    Nothing
    (listToAst [Literal (Integer 1), Symbol "b"])
    )