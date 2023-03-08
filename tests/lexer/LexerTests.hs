module LexerTests (lexerTestList) where
import Test.HUnit
import Cpt.LexerParser
import LibParser.Parser
import Cpt.Cpt ( Cpt(Assignement, Operation, Literal, Prototype, Identifier, Condition) )
import Cpt.Literal (Literal(..))


-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

lexerTestList :: Test
lexerTestList = TestList [testComment, testPrototype, testAssignement, testAssignementWithParams, testIdentifier, testWrongIdentifier, testOperation, testCondition]


-- Comment tests

testComment :: Test
testComment = TestCase (assertEqual "Correct comment"
    (Right (Assignement ("s",[],Operation [Literal (Int 1)]),""))
    (runParser pCpt "s = 1 \n--r\n")
    )

-- Prototype tests

testPrototype :: Test
testPrototype = TestCase (assertEqual "Correct prototype"
    (Right (Prototype ("pPrototype",["ParserCpt"]),""))
    (runParser pPrototype  "pPrototype :: ParserCpt")
    )

-- Assignement tests

testAssignement :: Test
testAssignement = TestCase (assertEqual "Correct assignement"
    (Right (Assignement ("s",[],Operation [Literal (Int 1)]),""))
    (runParser pAssignement "s = 1")
    )

testAssignementWithParams :: Test
testAssignementWithParams = TestCase (assertEqual "Correct assignement with params"
    (Right (Assignement ("s",[Identifier "a",Identifier "b"],Operation [Literal (Int 1)]),""))
    (runParser pAssignement "s a b = 1")
    )

-- Identifier tests

testIdentifier :: Test
testIdentifier = TestCase (assertEqual "Correct identifier"
    (Right ("s",""))
    (runParser pIdentifier "s")
    )

testWrongIdentifier :: Test
testWrongIdentifier = TestCase (assertEqual "Wrong identifier"
    (Left [])
    (runParser pIdentifier  "1")
    )

-- Operation tests

testOperation :: Test
testOperation = TestCase (assertEqual "Correct operation"
    (Right (Operation [Literal (Int 1),Literal (Int 2)],""))
    (runParser pOperation  "1 2")
    )

-- testOperation2 :: Test
-- testOperation2 = TestCase (assertEqual "Correct operation"
--     (Right (Operation [Literal (Int 1),(Cpt.Operator.Plus +),Operation [Literal (Int 1),(Cpt.Operator.Times *),Literal (Int 1)]],""))
--     (runParser pOperation "1  +(1* 1)")
--     )

-- Condition tests

testCondition :: Test
testCondition = TestCase (assertEqual "Correct condition"
    (Right (Condition (Literal (Int 1),Literal (Int 1),Literal (Int 1)),""))
    (runParser pCondition "if 1 then 1 else 1")
    )