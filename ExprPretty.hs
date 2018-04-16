module ExprPretty where

import ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

{-
  Instance Show Expr
    Provides a pretty representation of our datatype
    Matching the DSL provided in DiffExpr
-}

instance Show a => Show (Expr a) where
  show (Add e1 e2) = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Var ss) = parens $ "var \"" ++ ss ++ "\""
  show (Const x) = parens $ "val " ++ show x
  show (Cos ss) = parens $ "cos " ++ show ss
  show (Sin ss) = parens $ "sin " ++ show ss
  show (Log ss) = parens $ "log " ++ show ss
  show (Exp ss) = parens $ "exp " ++ show ss
  show (Ln ss) = parens $ "ln " ++ show ss
