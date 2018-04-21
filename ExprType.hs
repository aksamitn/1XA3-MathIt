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
            | Power (Expr a) (Expr a)
            | Cos (Expr a) -- Cosine
            | Sin (Expr a) -- Sin
            | Tan (Expr a) -- Tan
            | Csc (Expr a) -- Cosecant
            | Sec (Expr a) -- Secant
            | Cot (Expr a) -- Cotangent
            | Log (Expr a) (Expr a) -- Log
            | Exp (Expr a) -- Exponential
            | Ln (Expr a)  -- Natural Log
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
getVars (Power e1 e2) = getVars e1 `union` getVars e2
getVars (Cos x) = getVars x
getVars (Sin x) = getVars x
getVars (Tan x) = getVars x
getVars (Csc x) = getVars x
getVars (Sec x) = getVars x
getVars (Cot x) = getVars x
getVars (Log base x) = getVars base `union` getVars x
getVars (Exp x) = getVars x
getVars (Ln x) = getVars x
getVars (Const _) = []
getVars (Var ident) = [ident]
