module AstTests (astTestList) where

import Test.HUnit

import Ast (
    listToParams, Params, expressionToAst
    )
import Cpt.Cpt (
    Cpt (Literal, Identifier, Operation),
    )
import Cpt.Literal (Literal(Int))
import Error (GladosError (Ast), AstError (..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

astTestList :: Test
astTestList = TestList [listToParamsFullIdentifiers, listToParamsNotFullIdentifiers, listToAstInteger]

-- -------------------------------------------------------------------------- --
--                             listToParams Tests                             --
-- -------------------------------------------------------------------------- --

listToParamsFullIdentifiers :: Test
listToParamsFullIdentifiers = TestCase (assertEqual "For listToParams [a, b]"
    (Right ["a", "b"])
    (listToParams [Identifier "a", Identifier "b"])
    )

listToParamsNotFullIdentifiers :: Test
listToParamsNotFullIdentifiers  = TestCase (assertEqual "For listToParams [1, b]"
    (Left [Ast InvalidAst])
    (listToParams [Literal (Int 1), Identifier "b"])
    )

-- -------------------------------------------------------------------------- --
--                               listToAst Tests                              --
-- -------------------------------------------------------------------------- --

listToAstInteger :: Test
listToAstInteger = TestCase (assertEqual "For listToAst Integer"
    (Left [Ast InvalidAst])
    (expressionToAst [Literal (Int 1), Identifier "b"])
    )