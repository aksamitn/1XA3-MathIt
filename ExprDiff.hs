{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ExprDiff where

import ExprType
import qualified Data.Map as Map

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
      !+,!*,!^,cos',sin',tan',csc',sec',cot',log',ln',exp',var,val:
        are function wrappers for Expr
        constructors that perform
        additional simplification
-}

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  simplifiable :: Map.Map String a -> Expr a -> Bool
  partDiff :: String -> Expr a -> Expr a

  -- Addition operator
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList [] ) $ Add e1 e2

  -- Multiplication operator
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList [] ) $ Mult e1 e2

  -- Exponent operator
  (!^) :: Expr a -> Expr a -> Expr a
  e1 !^ e2 = simplify (Map.fromList []) $ Power e1 e2

  -- Cosine operator
  cos' :: Expr a -> Expr a
  cos' x = Cos x

  -- Sine operator
  sin' :: Expr a -> Expr a
  sin' x = Sin x

  -- Tangent operator
  tan' :: Expr a -> Expr a
  tan' x = Tan x

  -- Cosecant operator
  csc' :: Expr a -> Expr a
  csc' x = Csc x

  -- Secant operator
  sec' :: Expr a -> Expr a
  sec' x = Sec x

  -- Cotangent operator
  cot' :: Expr a -> Expr a
  cot' x = Cot x

  -- Logarithm operator
  log' :: Expr a -> Expr a -> Expr a
  log' base x = Log base x

  -- Natural logarithm operator
  ln' :: Expr a -> Expr a
  ln' x = Ln x

  -- Natural exponent operator
  exp' :: Expr a -> Expr a
  exp' x = Exp x

  -- Constants
  val :: a -> Expr a
  val x = Const x

  -- Variables
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
instance (Floating a, Eq a) => DiffExpr a where

  simplifiable vrs x = (simplify vrs x) /= x

  eval vrs (Add e1 e2) = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Power e1 e2) = (eval vrs e1) ** (eval vrs e2)
  eval vrs (Cos x) = cos (eval vrs x)
  eval vrs (Sin x) = sin (eval vrs x)
  eval vrs (Tan x) = tan (eval vrs x)
  eval vrs (Csc x) = 1/(sin (eval vrs x))
  eval vrs (Sec x) = 1/(cos (eval vrs x))
  eval vrs (Cot x) = 1/(tan (eval vrs x))
  eval vrs (Ln x) = log (eval vrs x)
  eval vrs (Log base x) = logBase (eval vrs base) (eval vrs x)
  eval vrs (Exp x) = exp (eval vrs x)
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v -> v
                       Nothing -> error "failed lookup in eval!" -- In simplify, if there's nothing you just return Var x cause there's no value for it

  simplify vrs (Const x) = Const x

  simplify vrs (Var x) = case Map.lookup x vrs of
                           Just v -> Const (eval vrs (Var x))
                           Nothing -> Var x

  simplify vrs (Power x y) = case (simplifiable vrs x) of
                                True -> case (simplifiable vrs y) of
                                          True -> simplify vrs (Power (simplify vrs x) (simplify vrs y))
                                          False -> Power (simplify vrs x) y
                                False -> case (simplifiable vrs y) of
                                           True -> Power x (simplify vrs y)
                                           False -> Power x y

  simplify vrs (Cos (Const x)) = Const (eval vrs (Cos (Const x)))
  simplify vrs (Cos x) = case (simplifiable vrs x) of
                           True -> simplify vrs (Cos (simplify vrs x))
                           False -> Cos (simplify vrs x)

  simplify vrs (Sin (Const x)) = Const (eval vrs (Sin (Const x)))
  simplify vrs (Sin x) = case (simplifiable vrs x) of
                            True -> simplify vrs (Sin (simplify vrs x))
                            False -> Sin (simplify vrs x)

  simplify vrs (Tan (Const x)) = Const (eval vrs (Tan (Const x)))
  simplify vrs (Tan x) = case (simplifiable vrs x) of
                            True -> simplify vrs (Tan (simplify vrs x))
                            False -> Tan (simplify vrs x)

  simplify vrs (Csc (Const x)) = Const (eval vrs (Csc (Const x)))
  simplify vrs (Csc x) = case (simplifiable vrs x) of
                            True -> simplify vrs (Csc (simplify vrs x))
                            False -> Csc (simplify vrs x)

  simplify vrs (Sec (Const x)) = Const (eval vrs (Sec (Const x)))
  simplify vrs (Sec x) = case (simplifiable vrs x) of
                            True -> simplify vrs (Sec (simplify vrs x))
                            False -> Sec (simplify vrs x)

  simplify vrs (Cot (Const x)) = Const (eval vrs (Cot (Const x)))
  simplify vrs (Cot x) = case (simplifiable vrs x) of
                            True -> simplify vrs (Cot (simplify vrs x))
                            False -> Cot (simplify vrs x)

  simplify vrs (Log (Const a) (Const b)) = Const (eval vrs (Log (Const a) (Const b)))
  simplify vrs (Log base x) = case (simplifiable vrs base) of
                                True -> case (simplifiable vrs x) of
                                          True -> simplify vrs (Log (simplify vrs base) (simplify vrs x))
                                          False -> Log (simplify vrs base) x
                                False -> case (simplifiable vrs x) of
                                           True -> Log base (simplify vrs x)
                                           False -> Log base x

  simplify vrs (Ln (Const x)) = Const (eval vrs (Ln (Const x)))
  simplify vrs (Ln x) = case (simplifiable vrs x) of
                            True -> simplify vrs (Ln (simplify vrs x))
                            False -> Ln (simplify vrs x)

  simplify vrs (Exp (Const x)) = Const (eval vrs (Exp (Const x)))
  simplify vrs (Exp x) = case (simplifiable vrs x) of
                            True -> simplify vrs (Exp (simplify vrs x))
                            False -> Exp (simplify vrs x)

  {-
    Addition Simplification:

      The add function may contain variables, and these variables may
      or may not have a value in the dictionary of values. There are
      two cases: either the variable has a value or it doesn't. Therefore,
      if the variable does not contain a value then preserve it as it cannot
      be simplified any further, but if the variable does have a value then
      call the eval function and attach the variable's value to the spot of
      where the variable was.

      Ex.
      2 + 2 = 4
      x + 5 = 5 + x            if x does not contain a value
      x + 5 = 5 + x = 9        if x = 4
  -}

  -- Simplify Addition of two constants by simply adding them together.
  simplify vrs (Add (Const a) (Const b)) = Const (eval vrs (Add (Const a) (Const b)))

  -- Simplify Addition of a Var and another Expr by first looking to see if
  -- the Var has an actual value, if yes then evaluate the Expr with the Var value and add.
  -- If not, then simply leave it as Var y and then simplify the other Expr.
  simplify vrs (Add (Var y) x) = case Map.lookup y vrs of
                                   Just v -> Const (eval vrs (Add (Var y) x))
                                   Nothing -> Add (Var y) x

  -- Simplify Addition of a Var and another Expr by first looking to see if
  -- the Var has an actual value, if yes then evaluate the Expr with the Var value and add.
  -- If not, then simply leave it as Var y and then simplify the other Expr.
  simplify vrs (Add x (Var y)) = case Map.lookup y vrs of
                                   Just v -> Const (eval vrs (Add x (Var y)))
                                   Nothing -> Add x (Var y)

  -- Simplify Addition of a Var and another Expr by first looking to see if
  -- the Var has an actual value, if yes then evaluate the Expr with the Var value and add.
  -- If not, then simply leave it as Var y and then simplify the other Expr.
  simplify vrs (Add (Var x) (Var y)) = case Map.lookup x vrs of
                                         Just v -> case Map.lookup y vrs of
                                                     Just s -> Const (eval vrs (Add (Var x) (Var y)))
                                                     Nothing -> Add (simplify vrs (Var x)) (simplify vrs (Var y))
                                         Nothing -> Add (Var x) (simplify vrs (Var y))

  -- Simplify Addition of a Var and another Var by first looking to see if
  -- the first Var has an actual value, if yes then evaluate the Var and check to see if
  -- the second Var has an actual value. If they both have values, then simply add them.
  -- If one has a value and not the other, simply reduce to the addition of a Const and a Var.
  simplify vrs (Add x y) = case ((simplifiable vrs x) || (simplifiable vrs y)) of
                             True -> simplify vrs (Add (simplify vrs x) (simplify vrs y))
                             False -> Add (simplify vrs x) (simplify vrs y)

  {-
    Multiplication Simplification:

      The mult function may contain variables, and these variables may
      or may not have a value in the dictionary of values. There are
      two cases: either the variable has a value or it doesn't. Therefore,
      if the variable does not contain a value then preserve it as it cannot
      be simplified any further, but if the variable does have a value then
      call the eval function and attach the variable's value to the spot of
      where the variable was.

      Ex.
      2 * 2 = 4
      x * 5 = 5 * x            if x does not contain a value
      x * 5 = 5 * x = 5        if x = 1
  -}

  -- Simplify Multiplication of two constants by simply multiplying them together.
  simplify vrs (Mult (Const a) (Const b)) = Const (eval vrs (Mult (Const a) (Const b)))

  -- Simplify Multiplication of a Var and another Expr by first looking to see if
  -- the Var has an actual value, if yes then evaluate the Expr with the Var value and multiply.
  -- If not, then simply leave it as Var y and then simplify the other Expr.
  simplify vrs (Mult (Var y) x) = case Map.lookup y vrs of
                                    Just v -> Const (eval vrs (Mult (Var y) x))
                                    Nothing -> Mult (Var y) (simplify vrs x)

  -- Simplify Multiplication of a Var and another Expr by first looking to see if
  -- the Var has an actual value, if yes then evaluate the Expr with the Var value and multiply.
  -- If not, then simply leave it as Var y and then simplify the other Expr.
  simplify vrs (Mult x (Var y)) = case Map.lookup y vrs of
                                    Just v -> Const (eval vrs (Mult x (Var y)))
                                    Nothing -> Mult x (Var y)

  -- Simplify Multiplication of a Var and another Var by first looking to see if
  -- the first Var has an actual value, if yes then evaluate the Var and check to see if
  -- the second Var has an actual value. If they both have values, then simply multiply them.
  -- If one has a value and not the other, simply reduce to the multiplication of a Const and a Var.
  simplify vrs (Mult (Var x) (Var y)) = case Map.lookup x vrs of
                                        Just v -> case Map.lookup y vrs of
                                                    Just s -> Const ((eval vrs (Add (Var x) (Var y))))
                                                    Nothing -> Add (Var x) (Var y)
                                        Nothing -> Add (simplify vrs (Var x)) (simplify vrs (Var y))

  -- Simplify Multiplication of two Exprs by first checking if they're simplifiable using the simplifiable function
  -- If yes, then simplify and evaluate the multiplication as much as possible
  -- If not, then simplify both Exprs as much as possible and return a Mult
  simplify vrs (Mult x y) = case ((simplifiable vrs x) || (simplifiable vrs y)) of
                             True -> simplify vrs (Mult (simplify vrs x) (simplify vrs y))
                             False -> Mult (simplify vrs x) (simplify vrs y)

  {-
    Partial Differentiation:

    The partDiff function performs partial differentiation according to a select
    variable input.

    There are two inputs:
      1 -> String input which represents the variable to differentiate according to.
           For example, an input of "x" will differentiate according to Var "x"
      2 -> Expr input which represents the expression that is differentiated
  -}

  partDiff _ (Const _) = Const 0 -- Any Constant differentiates to zero

  partDiff var (Var x) | x == var = (Const 1) -- For variables, first check if the variable string input matches the
                                              -- variable within the Expr, if yes then it equals one
                       | otherwise = (Const 0) -- If the variable and Expr Var don't match, it equals zero

  partDiff var (Ln x) = Mult (Power (Const (-1)) x) (partDiff var x) -- The derivative of ln is 1/x, however there is no division
                                                                     -- Expr, therefore Power must be used with -1 to add the same effect

  partDiff var (Log base x) = Mult (Power (Const (-1)) (Mult x (Ln base))) (partDiff var x) -- The derivative of log x at base b is
                                                                                            -- 1/(x * ln b), but there's no division Expr
                                                                                            -- therefore Power must be used with -1

  partDiff var (Sin x) = Mult (Cos x) (partDiff var x) --The derivative of sin x is cos x

  partDiff var (Cos x) = Mult (Mult (Const (-1)) (Sin x)) (partDiff var x) -- The derivative of cos x is -sin x

  partDiff var (Tan x) = Mult (Power (Sec x) (Const 2)) (partDiff var x) -- The derivative of tan x is (sec x)^2

  partDiff var (Csc x) = Mult (Mult (Mult (Const (-1)) (Csc x)) (Cot x)) (partDiff var x) -- The derivative of csc x is -csc x * cot x

  partDiff var (Sec x) = Mult (Mult (Sec x) (Tan x)) (partDiff var x) -- The derivative of sec x is sec x * tan x

  partDiff var (Cot x) = Mult (Mult (Const (-1)) (Power (Csc x) (Const 2))) (partDiff var x) -- The derivative of cot x is -((csc x)^2)

  partDiff var (Exp x) = Mult (Exp x) (partDiff var x) -- The derivative of e^x is e^x
