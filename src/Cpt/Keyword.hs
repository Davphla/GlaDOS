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

strToKeywords :: String -> Maybe Cpt.Keyword
strToKeywords "if" = Just Cpt.If
strToKeywords "then" = Just Cpt.Then
strToKeywords "else" = Just Cpt.Else
strToKeywords "lambda" = Just Cpt.Lambda
strToKeywords _ = Nothing
