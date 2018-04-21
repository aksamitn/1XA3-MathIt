module ExprPretty where

import ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

{-
  Instance Show Expr
    Provides a pretty representation of our datatype
    Matching the DSL provided in DiffExpr


  Uses the show instance and allows ghci print output of the Expr datatype
-}

instance Show a => Show (Expr a) where
  show (Add e1 e2) = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Power e1 e2) = parens (show e1) ++ " !^ " ++ parens (show e2)
  show (Var ss) = parens $ "var \"" ++ ss ++ "\""
  show (Const x) = parens $ "val " ++ show x
  show (Cos x) = parens $ "cos " ++ show x
  show (Sin x) = parens $ "sin " ++ show x
  show (Tan x) = parens $ "tan " ++ show x
  show (Csc x) = parens $ "csc " ++ show x
  show (Sec x) = parens $ "sec " ++ show x
  show (Cot x) = parens $ "cot " ++ show x
  show (Log base num) = parens $ "log " ++ show num ++ " of base " ++ show base
  show (Exp x) = parens $ "exp " ++ show x
  show (Ln x) = parens $ "ln " ++ show x
