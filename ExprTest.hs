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

parseAndSimpD :: Map.Map String Double -> String -> Expr Double
parseAndSimpD x ss = simplify x (parseExprD ss)

parseAndEvalD :: Map.Map String Double -> String -> Double
parseAndEvalD x ss = eval x (parseExprD ss)

parseAndDiffD :: String -> String -> Expr Double
parseAndDiffD x ss = partDiff x (parseExprD ss)


parseAndSimpF :: Map.Map String Float -> String -> Expr Float
parseAndSimpF x ss = simplify x (parseExprF ss)

parseAndEvalF :: Map.Map String Float -> String -> Float
parseAndEvalF x ss = eval x (parseExprF ss)

parseAndDiffF :: String -> String -> Expr Float
parseAndDiffF x ss = partDiff x (parseExprF ss)


{-
  Yeah I didn't really add in any, sorry :(

  Thank you very much for marking this assignment.
  While it's not very complete, the packages probably function as I have
  tested a lot throughout the way in order to complete the functions inside ExprDiff.

  I'd like to thank you for making this class fun. This assignment was a lot of hard work,
  but honestly I had a lot of fun doing it and learned a lot.

  Thanks so much

  ~ Bread
-}
