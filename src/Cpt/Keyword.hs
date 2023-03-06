{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Keyword.hs
-}

module Cpt.Keyword (Keyword, keywords, strToKeywords) where


data Keyword = If | Else | Then | Lambda deriving (Eq)
instance Show Keyword where
  show :: Keyword -> String
  show If = "if"
  show Else = "else"
  show Then = "then"
  show Lambda = "lambda"


keywords :: [String]
keywords = ["if", "then", "else", "lambda"]

strToKeywords :: String -> Maybe Keyword
strToKeywords "if" = Just If
strToKeywords "then" = Just Then
strToKeywords "else" = Just Else
strToKeywords "lambda" = Just Lambda
strToKeywords _ = Nothing
