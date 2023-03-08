{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Error.hs
-}

module Error (
  AstError (..),
  AstErrorReason (..),
  CptError (..),
  CptErrorReason (..),
  GladosError (..),
  GlobalWarning (..),
  ParseError (..)
  ) where


-- ------------------------------- Ast errors ------------------------------- --

data AstErrorReason
  = InvalidAstReason
  deriving (Eq)

instance Show AstErrorReason where
  show InvalidAstReason = "this is the reason"


data AstError
  = InvalidAst AstErrorReason String
  deriving (Eq)

instance Show AstError where
  show (InvalidAst r s) = "invalid usage of " ++ s ++ ": " ++ show r


-- ------------------------------- Cpt errors ------------------------------- --

data CptErrorReason
  = InvalidCptNotExpression
  | InvalidCptNotIdentifier
  | InvalidCptNotLiteral
  | InvalidCptNotKeyword
  | InvalidCptNotOperator
  | InvalidCptNotTreatable
  deriving (Eq)

instance Show CptErrorReason where
  show InvalidCptNotExpression = "not an expression"
  show InvalidCptNotIdentifier = "not an identifier"
  show InvalidCptNotLiteral = "not a literal"
  show InvalidCptNotKeyword = "not a keyword"
  show InvalidCptNotOperator = "not an operator"
  show InvalidCptNotTreatable = "should be an expression, assignment, prototype or operation"


data CptError = InvalidCpt CptErrorReason String
  deriving (Eq)

instance Show CptError where
  show (InvalidCpt r s) = "parse error on input: " ++ s ++ ": " ++ show r


-- ------------------------------ Parse errors ------------------------------ --

data ParseError = InvalidSyntax
  | Unexpected
  | UnexpectedEnd
  deriving (Eq, Show)


-- ------------------------------ Global errors ----------------------------- --

newtype GlobalWarning = NotImplemented String
  deriving (Eq)

instance Show GlobalWarning where
  show (NotImplemented s) = "not implemented: " ++ s


data GladosError
  = Cpt CptError
  | Ast AstError
  | Parser ParseError
  | Warning GlobalWarning
  deriving (Eq, Show)
