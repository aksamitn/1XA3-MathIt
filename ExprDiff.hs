{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ExprDiff where

import ExprType
import qualified Data.Map as Map
import ExprPretty

{-
  Class diffExpr:
    Differentiable Expressions
  ----------------------------

  This class has methods over the Expr datatype
  that assists with construction and evaluation
  of differentiable expressions

  ----------------------------------------------

  Methods:
    eval: takes a dictionary of variable identifiers
          and values, and uses it to compute the
          Expr fully
    simplify: takes a possibly incomplete dictionary
              and uses it to reduce Expr as much as
              possible

              e1 = x + y
              e2 = y + x
              e1 == e2

              Add (Add (Var "x") (Const 1) (Add (Const 2) (Var "y")))
              => Add (Const 3) (Add (Var "x") (Var "y"))
    partDiff: given a var identifier, differentiate
          in terms of that identifier

    Default Methods:
      !+,!*,var,val: are function wrappers for Expr
                     constructors that perform
                     additional simplification
-}

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a

  {- Default Methods -}
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList [] ) $ Add e1 e2

  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList [] ) $ Mult e1 e2

  val :: a -> Expr a
  val x = Const x

  var :: String -> Expr a
  var x = Var x

{-
  Most intuative instance of DiffExpr
  Num instance only relies on +,-
  Methods:
    eval: .....
    simplify:........
    partDiff:.....
-}
instance (Num a) => DiffExpr a where
  eval vrs (Add e1 e2) = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v -> v
                       Nothing -> error "failed lookup in eval!" -- In simplify, if there's nothing you just return Var x cause there's no value for it
  simplify _ e = e -- #TODO finish me
  partDiff _ e = e -- #TODO finish me
