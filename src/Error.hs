{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Error.hs
-}

module Error (AstError (..), CptError (..), GladosError (..), ParseError (..)) where


data AstError = InvalidAst | NotImplemented
  deriving (Eq, Show)

data CptError = InvalidCpt
  deriving (Eq, Show)

data ParseError = InvalidSynthax
  | Unexpected
  | UnexpectedEnd
  deriving (Eq, Show)

data GladosError
  = Cpt CptError
  | Ast AstError
  | Parser ParseError
  deriving (Eq, Show)
