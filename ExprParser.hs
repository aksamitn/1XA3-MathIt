module ExprParser (parseExprD,parseExprF) where

import Text.Parsec
import Text.Parsec.String

import ExprType

-- Main parsing function of doubles
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err -> error $ show err
                  Right expr -> expr

exprD :: Parser (Expr Double)
exprD =  try exprOpD <|> exprFuncsD <|>  exprConstD <|> exprVar

-- Improved Parsing of floats
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err -> error $ show err
                  Right expr -> expr

exprF :: Parser (Expr Float)
exprF =  try exprOpF <|> exprFuncsF <|>  exprConstF <|> exprVar


-- ############ IMPROVED EXPR PARSING FUNCTIONS FOR DOUBLES ############

exprFuncsD :: Parser (Expr Double)
exprFuncsD = do {
           s <- symbol "cos" <|>
                symbol "Cos" <|>
                symbol "sin" <|>
                symbol "Sin" <|>
                symbol "tan" <|>
                symbol "Tan" <|>
                symbol "csc" <|>
                symbol "Csc" <|>
                symbol "sec" <|>
                symbol "Sec" <|>
                symbol "cot" <|>
                symbol "Cot" <|>
                symbol "exp" <|>
                symbol "Exp" <|>
                symbol "ln"  <|>
                symbol "Ln"  <|>
                symbol "log" <|>
                symbol "Log";

           exprs <- (exprConstD <|> exprVar <|> exprFuncsD) <|> between (symbol "(") (symbol ")") (exprD);
           num <- optionMaybe ((try exprConstD <|> exprVar <|> exprFuncsD) <|> between (symbol "(") (symbol ")") (exprD));

           if s == "cos" || s == "Cos" then
             return (Cos exprs);
           else if s == "sin" || s == "Sin" then
             return (Sin exprs);
           else if s == "tan" || s == "Tan" then
             return (Tan exprs);
           else if s == "csc" || s == "Csc" then
             return (Csc exprs);
           else if s == "sec" || s == "Sec" then
             return (Sec exprs);
           else if s == "cot" || s == "Cot" then
             return (Cot exprs);
           else if s == "ln" || s == "Ln" then
             return (Ln exprs);
           else if s == "log" || s == "Log" then
             case num of
               Just x -> return (Log exprs x);
               Nothing -> error "Invalid log input, requires log (base) (value)";
           else
             return (Exp exprs);
         }

exprOpD :: Parser (Expr Double)
exprOpD = do {
                left <- (exprConstD <|> exprVar <|> exprFuncsD) <|> between (symbol "(") (symbol ")") (exprD);
                s <- (symbol "+" <|> symbol "*" <|> symbol "^");
                right <- (exprConstD <|> exprVar <|> exprFuncsD) <|> between (symbol "(") (symbol ")") (exprD);

                if s == "+" then
                  return (Add left right);
                else if s == "*" then
                  return (Mult left right);
                else
                  return (Power left right);
              }


exprConstD :: Parser (Expr Double)
exprConstD = do {
                   s <- double;
                   return (Const s);
                 }

-- ############ IMPROVED EXPR PARSING FUNCTIONS FOR FLOATS ############

exprFuncsF :: Parser (Expr Float)
exprFuncsF = do {
          s <- symbol "cos" <|>
               symbol "Cos" <|>
               symbol "sin" <|>
               symbol "Sin" <|>
               symbol "tan" <|>
               symbol "Tan" <|>
               symbol "csc" <|>
               symbol "Csc" <|>
               symbol "sec" <|>
               symbol "Sec" <|>
               symbol "cot" <|>
               symbol "Cot" <|>
               symbol "exp" <|>
               symbol "Exp" <|>
               symbol "ln"  <|>
               symbol "Ln"  <|>
               symbol "log" <|>
               symbol "Log";

          exprs <- (exprF) <|> between (symbol "(") (symbol ")") (exprF);
          num <- optionMaybe ((try exprFuncsF <|>  exprConstF <|> exprVar) <|> between (symbol "(") (symbol ")") (exprF));

          if s == "cos" || s == "Cos" then
            return (Cos exprs);
          else if s == "sin" || s == "Sin" then
            return (Sin exprs);
          else if s == "tan" || s == "Tan" then
            return (Tan exprs);
          else if s == "csc" || s == "Csc" then
            return (Csc exprs);
          else if s == "sec" || s == "Sec" then
            return (Sec exprs);
          else if s == "cot" || s == "Cot" then
            return (Cot exprs);
          else if s == "ln" || s == "Ln" then
            return (Ln exprs);
          else if s == "log" || s == "Log" then
            case num of
              Just x -> return (Log exprs x);
              Nothing -> error "Invalid log input, requires log (base) (value)";
          else
            return (Exp exprs);
        }

exprOpF :: Parser (Expr Float)
exprOpF = do {
               left <- (exprConstF <|> exprVar) <|> between (symbol "(") (symbol ")") (exprF);
               s <- symbol "+" <|> symbol "*" <|> symbol "^";
               right <- (exprConstF <|> exprVar) <|> between (symbol "(") (symbol ")") (exprF);

               if s == "+" then
                 return (Add left right);
               else if s == "*" then
                 return (Mult left right);
               else
                 return (Power left right)
             }


exprConstF :: Parser (Expr Float)
exprConstF = do {
                  s <- float;
                  return (Const s);
                }


-- ############ ABSTRACTED AND IMPROVED EXPR PARSING FUNCTIONS ############


exprVar :: Parser (Expr a)
exprVar = do {
                s <- between (symbol "'") (symbol "'") (many1 letter);
                return (Var s);
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
