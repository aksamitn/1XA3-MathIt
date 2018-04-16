module ExprType where

import Data.List

{-
  Expression Datatype
  -------------------

  Wraps different operations in a expression tree
  Ops:
    Add - standard binary addition
    Mult - standard binary multiplication
    Const - wrapper for a simple values
    Var - string identifier for variables
-}

-- | a datatype for encoding numeric expressions
data Expr a = Add (Expr a) (Expr a) -- ^ Binary Addition
            | Mult (Expr a) (Expr a) -- ^ Binary Multiplication
            | Cos (Expr a)
            | Sin (Expr a)
            | Log (Expr a)
            | Exp (Expr a)
            | Ln (Expr a)
            | Const a -- ^ Wrap a constant value
            | Var String -- ^ Wrap a variable identifier
  deriving Eq

{-
  getVars :
    retreieves variable identifiers
    an Expr
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2) = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _) = []
getVars (Var ident) = [ident]
