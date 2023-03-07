module AstTests (astTestList) where

import Test.HUnit

import Ast.Ast (
    listToParams, Params, expressionToAst
    )
import Cpt.Cpt (
    Cpt (Literal, Identifier, Operation),
    )
import Cpt.Literal (Literal(Int))
import Error (GladosError (Ast, Cpt), AstError (..), CptError (..), CptErrorReason (..))

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
    (Left [Cpt $ InvalidCpt InvalidCptNotExpression $ show (Literal (Int 1))])
    (listToParams [Literal (Int 1), Identifier "b"])
    )

-- -------------------------------------------------------------------------- --
--                               listToAst Tests                              --
-- -------------------------------------------------------------------------- --

listToAstInteger :: Test
listToAstInteger = TestCase (assertEqual "For listToAst Integer"
    (Left [Cpt $ InvalidCpt InvalidCptNotTreatable $ show [Literal (Int 1), Identifier "b"]])
    (expressionToAst [Literal (Int 1), Identifier "b"])
    )