module Lexer (lexerTestList) where
import Test.HUnit

import Parser (parseChar, parseAnyChar, parseOr, parseAnd, parseAndWith,
    parseMany, parseSome, parseUInt, parseInt,
    ParseError(..), ParseErrorContent(..))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

lexerTestList :: Test
lexerTestList = TestList [
    simpleParseChar, middleParseChar, notFoundParseChar, emptyStringParseChar,
    simpleParseAnyChar, middleParseAnyChar, notFoundParseAnyChar, firstParseOr,
    secondParseOr, noneParseOr, simpleParseAnd, firstParseAnd,
    secondParseAnd, noneParseAnd, simpleParseAndWith, simpleParseMany,
    noneParseMany, simpleParseSome, noneParseSome, simpleParseUInt,
    noneParseUInt, minusParseInt, plusParseInt, unsignedParseInt, noneParseInt
  ]

-- -------------------------------------------------------------------------- --
--                           Low level parser tests                           --
-- -------------------------------------------------------------------------- --

simpleParseChar :: Test
simpleParseChar = TestCase (assertEqual "For researched char at beginning of string"
    (parseChar 'a' "abc")
    (Right ('a', "bc"))
  )

middleParseChar :: Test
middleParseChar = TestCase (assertEqual "For researched char not at beginning of string"
    (parseChar 'a' "bac")
    (Left (ParseError 0 CharacterNotFound))
  )

notFoundParseChar :: Test
notFoundParseChar = TestCase (assertEqual "For researched char not in string"
    (parseChar 'a' "bc")
    (Left (ParseError 0 CharacterNotFound))
  )

emptyStringParseChar :: Test
emptyStringParseChar = TestCase (assertEqual "For researching in empty string"
    (parseChar 'a' "")
    (Left (ParseError 0 CharacterNotFound))
  )

simpleParseAnyChar :: Test
simpleParseAnyChar = TestCase (assertEqual "For any char at beginning of string"
    (parseAnyChar "ab" "abc")
    (Right ('a', "bc"))
  )

middleParseAnyChar :: Test
middleParseAnyChar = TestCase (assertEqual "For any char in middle at beginning of string"
    (parseAnyChar "bac" "abc")
    (Right ('a', "bc"))
  )

notFoundParseAnyChar :: Test
notFoundParseAnyChar = TestCase (assertEqual "For any char not in string"
    (parseAnyChar "abc" "xyz")
    (Left (ParseError 0 CharacterNotFound))
  )

-- -------------------------------------------------------------------------- --
--                          Higher level parser tests                         --
-- -------------------------------------------------------------------------- --

firstParseOr :: Test
firstParseOr = TestCase (assertEqual "For first parser of or working"
    (Right ('a', "bc"))
    (parseOr (parseChar 'a') (parseChar 'b') "abc")
  )

secondParseOr :: Test
secondParseOr = TestCase (assertEqual "For second parser of or working"
    (Right ('b', "ac"))
    (parseOr (parseChar 'a') (parseChar 'b') "bac")
  )

noneParseOr :: Test
noneParseOr = TestCase (assertEqual "For no parser of or working"
    (Left (ParseError 0 CharacterNotFound))
    (parseOr (parseChar 'a') (parseChar 'b') "xyz")
  )

simpleParseAnd :: Test
simpleParseAnd = TestCase (assertEqual "For both parsers of and working"
    (Right (('a', 'b'), "c"))
    (parseAnd (parseChar 'a') (parseChar 'b') "abc")
  )

firstParseAnd :: Test
firstParseAnd = TestCase (assertEqual "For first parser of and working"
    (Left (ParseError 0 CharacterNotFound))
    (parseAnd (parseChar 'a') (parseChar 'z') "abc")
  )

secondParseAnd :: Test
secondParseAnd = TestCase (assertEqual "For first parser of and not working"
    (parseAnd (parseChar 'z') (parseChar 'b') "abc")
    (Left (ParseError 0 CharacterNotFound))
  )

noneParseAnd :: Test
noneParseAnd = TestCase (assertEqual "For no parser of and working"
    (parseAnd (parseChar 'a') (parseChar 'b') "xyz")
    (Left (ParseError 0 CharacterNotFound))
  )

simpleParseAndWith :: Test
simpleParseAndWith = TestCase (assertEqual "For both parsers of andWith working"
    (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b') "abcd")
    (Right ("ab", "cd"))
  )

simpleParseMany :: Test
simpleParseMany = TestCase (assertEqual "For parser of many working"
    (parseMany (parseChar 'a') "aaabc")
    (Right ("aaa", "bc"))
  )

noneParseMany :: Test
noneParseMany = TestCase (assertEqual "For parser of many not working"
    (parseMany (parseChar 'a') "bcaaa")
    (Right ("", "bcaaa"))
  )

simpleParseSome :: Test
simpleParseSome = TestCase (assertEqual "For parser of some working"
    (parseSome (parseChar 'a') "aaabc")
    (Right ("aaa", "bc"))
  )

noneParseSome :: Test
noneParseSome = TestCase (assertEqual "For parser of some not working"
    (parseSome (parseChar 'a') "bcaaa")
    (Left (ParseError 0 CharacterNotFound))
  )

-- -------------------------------------------------------------------------- --
--                           Combinator parser tests                          --
-- -------------------------------------------------------------------------- --

simpleParseUInt :: Test
simpleParseUInt = TestCase (assertEqual "For parser of uint working"
    (parseUInt "123abc")
    (Right (123, "abc"))
  )

noneParseUInt :: Test
noneParseUInt = TestCase (assertEqual "For parser of uint not working"
    (parseUInt "abc")
    (Left (ParseError 0 CharacterNotFound))
  )

minusParseInt :: Test
minusParseInt = TestCase (assertEqual "For parser of minus int working"
    (parseInt "-123abc")
    (Right (-123, "abc"))
  )

plusParseInt :: Test
plusParseInt = TestCase (assertEqual "For parser of plus int working"
    (parseInt "+123abc")
    (Right (123, "abc"))
  )

unsignedParseInt :: Test
unsignedParseInt = TestCase (assertEqual "For parser of int working"
    (parseInt "123abc")
    (Right (123, "abc"))
  )

noneParseInt :: Test
noneParseInt = TestCase (assertEqual "For parser of int not working"
    (parseInt "abc")
    (Left (ParseError 0 CharacterNotFound))
  )