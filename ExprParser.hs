module ExprParser (parseExprD,parseExprF) where

import Text.Parsec
import Text.Parsec.String

import ExprType
import ExprPretty

{-
  parseExpr*
  ----------

  Takes a string of format:
  .....
  and parses an expression of Expr *
-}

parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err -> error $ show err
                  Right expr -> expr

parseExprF :: String -> Expr Double
parseExprF ss = case parse exprD "" ss of
                  Left err -> error $ show err
                  Right expr -> expr

exprD :: Parser (Expr Double)
exprD = do {
                s <- symbol "Add" <|> symbol "Mult";
                spaces;
                ss <- between (symbol "(") (symbol ")") (exprVar <|> exprConstD <|> exprOpD <|> exprD);
                spaces;
                ss' <- between (symbol "(") (symbol ")") (exprVar <|> exprConstD <|> exprOpD <|> exprD);

                {-
                  This parser can only have the option of
                  Adding or Multiplying, so it's necessary
                  to check whether the initial input was
                  "Add" or "Mult", and return the correct
                  result accordingly
                -}
                if s == "Add" then
                  return (Add ss ss'); -- Initial s is "Add", return Add
                else -- The only other option is to return "Mult", since it wasn't "Add"
                  return (Mult ss ss')
              }

exprF :: Parser (Expr Float)
exprF = do {
                s <- symbol "Add" <|> symbol "Mult";
                spaces;
                ss <- between (symbol "(") (symbol ")") (exprVar <|> exprConstF <|> exprF);
                spaces;
                ss' <- between (symbol "(") (symbol ")") (exprVar <|> exprConstF <|> exprF);

                {-
                  This parser can only have the option of
                  Adding or Multiplying, so it's necessary
                  to check whether the initial input was
                  "Add" or "Mult", and return the correct
                  result accordingly
                -}
                if s == "Add" then
                  return (Add ss ss'); -- Initial s is "Add", return Add
                else -- The only other option is to return "Mult", since it wasn't "Add"
                  return (Mult ss ss')
              }

exprConstD :: Parser (Expr Double)
exprConstD = do {
               symbol "Const";
               spaces;
               ss <- double;
               return (Const ss);
             }

exprOpD :: Parser (Expr Double)
exprOpD = do {
               symbol "Cos" <|> symbol "Sin" <|> symbol "Log" <|> symbol "Exp";
               spaces;
               ss <- exprD;
               return (Cos ss)
             }

exprConstF :: Parser (Expr Float)
exprConstF = do {
              symbol "Const";
              spaces;
              ss <- float;
              return (Const ss);
            }

exprVar :: Parser (Expr a)
exprVar = do {
               symbol "Var";
               spaces;
               ss <- many1 letter;
               return (Var ss);
             }





-- Utility Parsers

parens :: Parser a -> Parser a
parens p = do { char '(';
               cs <- p;
               char ')';
               return cs }

symbol :: String -> Parser String
symbol ss = let
 symbol' :: Parser String
 symbol' = do { spaces;
                ss' <- string ss;
                spaces;
                return ss' }
 in try symbol'

removeRight (Right ss) = ss

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-";
                dig <- digits;
                return (neg ++ dig) }

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

decimalDigits :: Parser String
decimalDigits = do { d <- char '.';
                     rm <- digits;
                     return $ d:rm }

decimalDigits' :: Parser String
decimalDigits' = do { ds <- try negDigits <|> digits;
                   rs <- try decimalDigits <|> return "";
                   return $ ds ++ rs }

double :: Parser Double
double = fmap read $ decimalDigits'

float :: Parser Float
float = fmap read $ decimalDigits'
