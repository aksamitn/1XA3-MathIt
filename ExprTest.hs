module ExprTest where

import ExprType
import ExprDiff
import ExprParser
import ExprPretty

import qualified Data.Map as Map

sampleExpr1 :: Expr Double
sampleExpr1 = (var "x") !+ (var "y")

listToExpr1 :: [Double] -> Expr Double
listToExpr1 xs = undefined


--exprProp :: Expr Double -> Bool

--instance Arbitrary (Expr a) where
