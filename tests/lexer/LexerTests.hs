module LexerTests (lexerTestList) where
import Test.HUnit

import Cpt (Cpt (..))
import Lexer (runParser, pCpt, pLisp)
import Literal (Literal (..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

lexerTestList :: Test
lexerTestList = TestList [
    simpleValueLisp, whitespacesValueLisp
  ]

-- -------------------------------------------------------------------------- --
--                              Lisp parser tests                             --
-- -------------------------------------------------------------------------- --

simpleValueLisp :: Test
simpleValueLisp = TestCase (assertEqual "For single value expression"
    (Right ([Literal (Integer 123)], ""))
    (runParser pLisp "123")
  )

whitespacesValueLisp :: Test
whitespacesValueLisp = TestCase (assertEqual "For single value with whitespaces"
    (Right ([Literal (Integer 123)], ""))
    (runParser pLisp "  123  ")
  )

listValuesLisp :: Test
listValuesLisp = TestCase (assertEqual "For list of values"
    (Right ([Literal (Integer 123), Literal (Integer 456)], ""))
    (runParser pLisp "(123 456)")
  )