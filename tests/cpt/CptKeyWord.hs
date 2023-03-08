module CptKeyWord (cptKeywordTestList) where

import Test.HUnit

import Cpt.Keyword

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptKeywordTestList :: Test
cptKeywordTestList = TestList [strToKeywordsIf, strToKeywordsThen, strToKeywordsElse, 
    strToKeywordsLambda, strToKeywordsNothing, keywordsList]

-- -------------------------------------------------------------------------- --
--                                strToKeywords                               --
-- -------------------------------------------------------------------------- --

strToKeywordsIf :: Test
strToKeywordsIf = TestCase (assertEqual "For strToKeywords for if"
    (Just If)
    (strToKeywords "if")
    )

strToKeywordsThen :: Test
strToKeywordsThen = TestCase (assertEqual "For strToKeywords for then"
    (Just Then)
    (strToKeywords "then")
    )

strToKeywordsElse :: Test
strToKeywordsElse = TestCase (assertEqual "For strToKeywords for else"
    (Just Else)
    (strToKeywords "else")
    )

strToKeywordsLambda :: Test
strToKeywordsLambda = TestCase (assertEqual "For strToKeywords for lambda"
    (Just Lambda)
    (strToKeywords "lambda")
    )

strToKeywordsNothing :: Test
strToKeywordsNothing = TestCase (assertEqual "For strToKeywords for nothing"
    Nothing
    (strToKeywords "Nothing")
    )

-- -------------------------------------------------------------------------- --
--                                  keywords                                  --
-- -------------------------------------------------------------------------- --

keywordsList :: Test
keywordsList = TestCase (assertEqual "For keywords"
    ["if", "then", "else", "lambda"]
    keywords
    )