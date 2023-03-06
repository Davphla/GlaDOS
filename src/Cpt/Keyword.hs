{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Keyword.hs
-}

module Cpt.Keyword (keywords, strToKeywords) where

import Cpt.Cpt ( Keyword(..) )

keywords :: [String]
keywords = ["if", "then", "else", "lambda"]

strToKeywords :: String -> Maybe Keyword
strToKeywords "if" = Just If
strToKeywords "then" = Just Then
strToKeywords "else" = Just Else
strToKeywords "lambda" = Just Lambda
strToKeywords _ = Nothing
